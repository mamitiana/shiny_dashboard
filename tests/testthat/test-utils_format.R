test_that("fmt_pct formats a proportion and handles non-finite input", {
  expect_equal(fmt_pct(0.123), "12.3%")
  expect_equal(fmt_pct(0), "0.0%")
  expect_true(is.na(fmt_pct(NA_real_)))
  expect_true(is.na(fmt_pct(NaN)))          # e.g. a 0/0 rate
  expect_true(is.na(fmt_pct(Inf)))
})

test_that("fmt_currency formats an amount and handles non-finite input", {
  expect_equal(fmt_currency(12345), "$12,345")
  expect_equal(fmt_currency(0), "$0")
  expect_true(is.na(fmt_currency(NA_real_)))
})

test_that("fmt_number formats a count and handles non-finite input", {
  expect_equal(fmt_number(12345), "12,345")
  expect_true(is.na(fmt_number(NA_real_)))
})

test_that("label_lookup returns known labels and title-cases unknown keys", {
  expect_equal(label_lookup("region"), "Region")
  expect_equal(label_lookup("par30"), "PAR 30")
  expect_equal(label_lookup("some_new_field"), "Some New Field")
  expect_equal(label_lookup(c("region", "some_new_field")), c("Region", "Some New Field"))
})

test_that("n_too_small applies the default threshold and treats NA as too small", {
  expect_true(n_too_small(5))
  expect_true(n_too_small(NA_integer_))
  expect_false(n_too_small(30))
  expect_false(n_too_small(50))
  expect_true(n_too_small(20, min_n = 25))
})
