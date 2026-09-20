test_that("ks_statistic and roc_auc are 1 under perfect separation", {
  good_scores <- 700:709
  bad_scores <- 300:309
  score <- c(good_scores, bad_scores)
  bad_flag <- c(rep(FALSE, length(good_scores)), rep(TRUE, length(bad_scores)))

  expect_equal(roc_auc(score, bad_flag), 1)
  expect_equal(ks_statistic(score, bad_flag)$ks, 1)
})

test_that("roc_auc does not overflow on a portfolio-sized class imbalance", {
  # n_bad * n_good exceeds .Machine$integer.max around 50k good vs 50k bad;
  # a real portfolio (250k loans, imbalanced) crosses it easily.
  set.seed(1)
  n <- 250000
  bad_flag <- stats::rbinom(n, 1, 0.1) == 1
  score <- stats::rnorm(n) - ifelse(bad_flag, 0.5, 0)
  result <- roc_auc(score, bad_flag)
  expect_true(is.finite(result))
  expect_true(result > 0.5 && result < 1)
})

test_that("ks_statistic and roc_auc return NA when one class is empty", {
  score <- 300:310
  all_good <- rep(FALSE, length(score))

  expect_true(is.na(roc_auc(score, all_good)))
  expect_true(is.na(ks_statistic(score, all_good)$ks))
})

test_that("score_distribution_by_outcome handles an empty input", {
  empty <- tibble::tibble(score = double(0), bad_flag = logical(0))
  result <- score_distribution_by_outcome(empty)
  expect_equal(nrow(result), 0)
  expect_named(result, c("band", "n_loans", "bad_rate", "too_small"))
})

test_that("score_distribution_by_outcome flags bands with too few loans", {
  loans <- tibble::tibble(
    score = c(rep(400, 5), rep(800, 40)),
    bad_flag = c(rep(TRUE, 2), rep(FALSE, 3), rep(FALSE, 40))
  )
  result <- score_distribution_by_outcome(loans)
  band_e <- result[result$band == "E", ]
  band_a <- result[result$band == "A", ]
  expect_true(band_e$too_small)
  expect_false(band_a$too_small)
})

test_that("discrimination_by_segment marks small segments instead of computing a false-precision AUC", {
  loans <- tibble::tibble(
    score = c(300:309, 700:749),
    bad_flag = c(rep(TRUE, 10), rep(FALSE, 50)),
    region = c(rep("Tiny", 10), rep("Big", 50))
  )
  result <- discrimination_by_segment(loans, "region")
  tiny <- result[result$region == "Tiny", ]
  big <- result[result$region == "Big", ]
  expect_true(tiny$too_small)
  expect_true(is.na(tiny$auc))
  expect_false(big$too_small)
})
