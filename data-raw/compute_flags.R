# data-raw/compute_flags.R
#
# Precomputes AML transaction-monitoring flags once for the whole
# portfolio. flag_structuring() and friends (R/fct_portfolio.R) scan every
# loan's transaction history; recomputing that on every portfolio-view
# filter change would blow past the app's 1-second repaint target at 250k
# loans, so it runs here instead - the app just filters the precomputed
# result by loan_id (see PERFORMANCE TARGETS in CLAUDE.md).
#
# Must be run after generate_synthetic.R. Run from the project root:
#   source("data-raw/compute_flags.R")

source("R/fct_portfolio.R")

transactions <- readRDS("data/transactions.rds")

aml_flags <- list(
  structuring = flag_structuring(transactions),
  velocity = flag_velocity(transactions),
  unusual_counterparty = flag_unusual_counterparty(transactions)
)

saveRDS(aml_flags, "data/aml_flags.rds")

message(sprintf(
  "Flagged %s structuring, %s velocity, %s unusual-counterparty loans/transactions",
  nrow(aml_flags$structuring), nrow(aml_flags$velocity), nrow(aml_flags$unusual_counterparty)
))
