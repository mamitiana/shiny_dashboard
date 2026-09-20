fit_toy_model <- function() {
  # Deliberately not perfectly separable (a couple of labels break the
  # age/amount trend) so glm() doesn't warn about fitted probabilities of
  # 0/1.
  df <- tibble::tibble(
    bad_flag = c(0, 1, 0, 0, 1, 1, 0, 1, 0, 1),
    age = c(20, 65, 25, 60, 30, 55, 35, 50, 40, 45),
    amount = c(100, 5000, 200, 4500, 300, 4000, 3500, 400, 500, 3000)
  )
  model <- stats::glm(bad_flag ~ age + amount, data = df, family = stats::binomial())
  list(model = model, scale = list(slope = -50, intercept = 600))
}

test_that("score_applicant returns one row within the 300-850 range", {
  fit <- fit_toy_model()
  applicant <- tibble::tibble(age = 40, amount = 1000)
  result <- score_applicant(fit$model, applicant, fit$scale)

  expect_equal(nrow(result), 1)
  expect_true(result$score >= 300 && result$score <= 850)
  expect_true(result$pd >= 0 && result$pd <= 1)
  expect_type(result$band, "character")
})

test_that("score_contributions returns one row per model term, ordered by magnitude", {
  fit <- fit_toy_model()
  applicant <- tibble::tibble(age = 40, amount = 1000)
  result <- score_contributions(fit$model, applicant, fit$scale)

  expect_setequal(result$variable, c("age", "amount"))
  expect_true(all(diff(abs(result$contribution)) <= 0))
})

test_that("explain_score reports the historical rate when the band has enough loans", {
  msg <- explain_score("C", 0.12, 1204)
  expect_match(msg, "band C")
  expect_match(msg, "12.0%")
  expect_match(msg, "1,204")
})

test_that("explain_score defers to a small-sample warning instead of a false-precision rate", {
  msg <- explain_score("E", 0.5, 4)
  expect_match(msg, "too few historical loans")
  expect_false(grepl("50.0%", msg))
})

test_that("explain_score handles a band outside the defined ranges", {
  msg <- explain_score(NA_character_, NA_real_, NA_integer_)
  expect_match(msg, "outside the defined bands")
})
