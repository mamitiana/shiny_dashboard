# data-raw/generate_synthetic.R
#
# Builds the synthetic loan book used by the whole app. Seeded and
# reproducible - re-running this script always produces the same data.
# No client data is used anywhere in this repository; every value below is
# sampled from parametric distributions with hand-picked segment effects.
#
# Writes:
#   data/loans.rds        - one row per loan
#   data/transactions.rds - one row per transaction, keyed by loan_id
#
# Run from the project root: source("data-raw/generate_synthetic.R")

set.seed(20240115)

n_loans <- 250000

regions <- c("Abidjan", "Bouake", "Yamoussoukro", "San-Pedro", "Korhogo", "Daloa")
region_risk <- stats::setNames(c(-0.10, 0.05, -0.05, 0.15, 0.20, 0.10), regions)
region_prob <- stats::setNames(c(0.30, 0.18, 0.12, 0.16, 0.14, 0.10), regions)

branches_per_region <- 4
branches <- unlist(lapply(regions, function(r) paste0(r, "-", seq_len(branches_per_region))))
branch_region <- stats::setNames(rep(regions, each = branches_per_region), branches)
# Some branches are simply more diligent about KYC than others.
branch_kyc_quality <- stats::setNames(stats::rbeta(length(branches), 6, 2), branches)

products <- c("Micro Business", "Agri", "Consumer", "Group Loan")
product_risk <- stats::setNames(c(0.00, 0.10, -0.05, -0.15), products)
product_prob <- stats::setNames(c(0.35, 0.20, 0.30, 0.15), products)
product_amount_mean <- stats::setNames(c(1500, 2200, 800, 600), products)

loan_id <- sprintf("L%07d", seq_len(n_loans))
age <- pmin(pmax(round(stats::rnorm(n_loans, mean = 38, sd = 11)), 18), 75)
sex <- sample(c("Female", "Male"), n_loans, replace = TRUE, prob = c(0.62, 0.38))
region <- sample(regions, n_loans, replace = TRUE, prob = region_prob)

# Assign branches within region (looping over the 6 regions, not the 250k
# rows, to keep this fast).
branch <- character(n_loans)
for (r in regions) {
  idx <- which(region == r)
  branch[idx] <- sample(branches[branch_region == r], length(idx), replace = TRUE)
}

product <- sample(products, n_loans, replace = TRUE, prob = product_prob)

amount <- round(stats::rlnorm(n_loans, meanlog = log(product_amount_mean[product]), sdlog = 0.5) / 10) * 10
amount <- pmin(pmax(amount, 100), 20000)

term_months <- sample(c(6, 12, 18, 24), n_loans, replace = TRUE, prob = c(0.35, 0.35, 0.2, 0.1))
disbursement_date <- as.Date("2022-01-01") + sample(0:(365 * 3), n_loans, replace = TRUE)

# Latent default probability: logistic function of age, amount, region and
# product plus noise, so segments show genuinely different risk levels for
# the validation/PAR charts to display.
age_effect <- -0.015 * (age - 38)
amount_effect <- 0.00015 * (amount - 1200)
region_effect <- region_risk[region]
product_effect <- product_risk[product]
sex_effect <- ifelse(sex == "Female", -0.10, 0.10)
noise <- stats::rnorm(n_loans, 0, 0.6)

linpred_true <- -1.6 + age_effect + amount_effect + region_effect + product_effect + sex_effect + noise
pd_true <- stats::plogis(linpred_true)
bad_flag <- stats::rbinom(n_loans, 1, pd_true) == 1

# Days past due: bad loans get a right-skewed draw at/above 30; good loans
# stay under 30 with most at/near zero.
dpd <- ifelse(
  bad_flag,
  pmin(round(stats::rgamma(n_loans, shape = 2, scale = 45) + 30), 365),
  pmin(pmax(round(stats::rgamma(n_loans, shape = 1.2, scale = 5)), 0), 29)
)

outstanding_balance <- round(amount * stats::runif(n_loans, 0.1, 1))

kyc_id_doc <- stats::rbinom(n_loans, 1, branch_kyc_quality[branch]) == 1
kyc_proof_address <- stats::rbinom(n_loans, 1, branch_kyc_quality[branch] * 0.9) == 1
kyc_phone_verified <- stats::rbinom(n_loans, 1, branch_kyc_quality[branch] * 0.95) == 1
kyc_income_verified <- stats::rbinom(n_loans, 1, branch_kyc_quality[branch] * 0.8) == 1
kyc_next_of_kin <- stats::rbinom(n_loans, 1, branch_kyc_quality[branch] * 0.85) == 1

amount_band <- as.character(cut(
  amount,
  breaks = c(0, 500, 1000, 2500, 5000, Inf),
  labels = c("<500", "500-1k", "1k-2.5k", "2.5k-5k", "5k+"),
  right = FALSE
))

loans <- tibble::tibble(
  loan_id, age, sex, region, branch, product, amount, amount_band, term_months,
  disbursement_date, outstanding_balance, dpd, bad_flag,
  kyc_id_doc, kyc_proof_address, kyc_phone_verified, kyc_income_verified, kyc_next_of_kin,
  pd_true
)

# --- Transactions -----------------------------------------------------
# A handful of repayment/withdrawal-style transactions per loan, plus a
# small injected minority of loans showing structuring and velocity
# patterns so the AML rule functions in fct_portfolio.R have genuine
# signal to detect.

n_txn_per_loan <- pmax(stats::rpois(n_loans, lambda = 3), 1)
txn_loan_id <- rep(loan_id, n_txn_per_loan)
txn_disb_date <- rep(disbursement_date, n_txn_per_loan)
n_txns <- length(txn_loan_id)

txn_date <- txn_disb_date + sample(0:180, n_txns, replace = TRUE)
txn_amount <- round(stats::rlnorm(n_txns, meanlog = log(300), sdlog = 0.7))
counterparty_id <- sprintf("CP%05d", sample(1:3000, n_txns, replace = TRUE))

transactions <- tibble::tibble(
  loan_id = txn_loan_id,
  txn_date = txn_date,
  amount = txn_amount,
  counterparty_id = counterparty_id
)

structuring_loans <- sample(loan_id, size = round(n_loans * 0.01))
structuring_txns <- purrr::map_dfr(structuring_loans, function(lid) {
  base_date <- disbursement_date[loan_id == lid][1] + sample(10:150, 1)
  tibble::tibble(
    loan_id = lid,
    txn_date = base_date + 0:3,
    amount = round(stats::runif(4, 8000, 9900)),
    counterparty_id = sprintf("CP%05d", sample(1:3000, 1))
  )
})
transactions <- dplyr::bind_rows(transactions, structuring_txns)

velocity_loans <- sample(setdiff(loan_id, structuring_loans), size = round(n_loans * 0.01))
velocity_txns <- purrr::map_dfr(velocity_loans, function(lid) {
  base_date <- disbursement_date[loan_id == lid][1] + sample(10:150, 1)
  tibble::tibble(
    loan_id = lid,
    txn_date = rep(base_date, 12),
    amount = round(stats::runif(12, 20, 200)),
    counterparty_id = sprintf("CP%05d", sample(1:3000, 12, replace = TRUE))
  )
})
transactions <- dplyr::bind_rows(transactions, velocity_txns)

dir.create("data", showWarnings = FALSE)
saveRDS(loans, "data/loans.rds")
saveRDS(transactions, "data/transactions.rds")

message(sprintf("Wrote %s loans and %s transactions to data/", nrow(loans), nrow(transactions)))
