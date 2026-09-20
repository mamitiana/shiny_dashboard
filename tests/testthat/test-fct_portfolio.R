test_that("score_to_band assigns the correct band and NA outside all ranges", {
  expect_equal(score_to_band(c(800, 690, 620, 550, 350, 100)),
               c("A", "B", "C", "D", "E", NA_character_))
})

test_that("par_summary computes PAR rates on a normal portfolio", {
  loans <- tibble::tibble(
    outstanding_balance = c(100, 100, 100, 100),
    dpd = c(0, 40, 70, 100)
  )
  result <- par_summary(loans)
  expect_equal(result$par30, 3 / 4)
  expect_equal(result$par60, 2 / 4)
  expect_equal(result$par90, 1 / 4)
  expect_equal(result$n_loans, 4)
})

test_that("par_summary avoids division by zero when outstanding balance is all zero", {
  loans <- tibble::tibble(outstanding_balance = c(0, 0), dpd = c(40, 0))
  result <- par_summary(loans)
  expect_true(is.na(result$par30))
})

test_that("par_summary handles a single-row portfolio", {
  loans <- tibble::tibble(outstanding_balance = 500, dpd = 45)
  result <- par_summary(loans)
  expect_equal(result$par30, 1)
  expect_equal(result$par90, 0)
})

test_that("par_trend returns an empty tibble with the expected columns for no loans", {
  loans <- tibble::tibble(disbursement_date = as.Date(character(0)),
                           outstanding_balance = double(0), dpd = double(0))
  result <- par_trend(loans)
  expect_equal(nrow(result), 0)
  expect_true(all(c("period", "par30", "par60", "par90") %in% names(result)))
})

test_that("disbursement_breakdown handles an empty filter result", {
  loans <- tibble::tibble(region = character(0), amount = double(0))
  result <- disbursement_breakdown(loans, "region")
  expect_equal(nrow(result), 0)
})

test_that("disbursement_breakdown computes shares correctly", {
  loans <- tibble::tibble(region = c("A", "A", "B"), amount = c(100, 100, 200))
  result <- disbursement_breakdown(loans, "region")
  a_row <- result[result$region == "A", ]
  expect_equal(a_row$n_loans, 2)
  expect_equal(a_row$share_of_loans, 2 / 3)
  expect_equal(a_row$share_of_amount, 200 / 400)
})

test_that("kyc_completeness handles an all-missing KYC column", {
  loans <- tibble::tibble(
    branch = c("X", "X"),
    kyc_id_doc = c(NA, NA),
    kyc_proof_address = c(TRUE, FALSE),
    kyc_phone_verified = c(TRUE, TRUE),
    kyc_income_verified = c(TRUE, TRUE),
    kyc_next_of_kin = c(TRUE, TRUE)
  )
  result <- kyc_completeness(loans)
  expect_true(is.nan(result$kyc_id_doc) || is.na(result$kyc_id_doc))
  expect_equal(result$kyc_proof_address, 0.5)
})

test_that("flag_structuring detects a qualifying pattern and ignores an insufficient one", {
  d0 <- as.Date("2024-01-01")
  transactions <- tibble::tibble(
    loan_id = c("A", "A", "A", "B", "B"),
    txn_date = c(d0, d0 + 1, d0 + 2, d0, d0 + 1),
    amount = c(4000, 4000, 4000, 4000, 4000),
    counterparty_id = "CP1"
  )
  result <- flag_structuring(transactions)
  expect_equal(nrow(result), 1)
  expect_equal(result$loan_id, "A")
})

test_that("flag_structuring handles an empty transaction table", {
  transactions <- tibble::tibble(loan_id = character(0), txn_date = as.Date(character(0)),
                                  amount = double(0), counterparty_id = character(0))
  result <- flag_structuring(transactions)
  expect_equal(nrow(result), 0)
})

test_that("flag_velocity flags a high same-day transaction count", {
  d0 <- as.Date("2024-01-01")
  transactions <- tibble::tibble(
    loan_id = rep("A", 11),
    txn_date = rep(d0, 11),
    amount = 1:11,
    counterparty_id = "CP1"
  )
  result <- flag_velocity(transactions)
  expect_equal(nrow(result), 1)
  expect_equal(result$n_txns, 11)
})

test_that("flag_unusual_counterparty flags a lone outlier and skips single-txn loans", {
  transactions <- tibble::tibble(
    loan_id = c(rep("A", 10), "A", "B"),
    counterparty_id = c(rep("CP1", 10), "CP2", "CP1"),
    amount = c(rep(100, 10), 200, 100)
  )
  result <- flag_unusual_counterparty(transactions)
  expect_equal(nrow(result), 1)
  expect_equal(result$counterparty_id, "CP2")
  expect_false("B" %in% result$loan_id)
})
