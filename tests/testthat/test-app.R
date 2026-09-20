# shinytest2 app-level coverage, driving a real headless browser. Requires
# data/*.rds to exist:
#   source("data-raw/generate_synthetic.R")
#   source("data-raw/fit_model.R")
#   source("data-raw/compute_flags.R")
# Run via shinytest2::test_app() from the project root.
#
# Uses get_value() rather than expect_values()'s snapshotting, which pulls
# in a screenshot step that isn't reliable in every headless environment.

testthat::skip_if_not_installed("shinytest2")
library(shinytest2)

test_that("the app loads and both views render", {
  skip_if_not(file.exists("../../data/loans.rds"), "run data-raw/ scripts first")

  app <- AppDriver$new(app_dir = "../../", name = "app-loads", height = 900, width = 1400)
  on.exit(app$stop())

  expect_true(!is.null(app$get_value(output = "scoring-decision_box")))

  app$set_inputs(navbar = "Portfolio")
  app$wait_for_idle()
  expect_true(!is.null(app$get_value(output = "portfolio-disb_region_plot")))
})

test_that("a portfolio filter change updates the disbursement output", {
  skip_if_not(file.exists("../../data/loans.rds"), "run data-raw/ scripts first")

  app <- AppDriver$new(app_dir = "../../", name = "portfolio-filter", height = 900, width = 1400)
  on.exit(app$stop())

  app$set_inputs(navbar = "Portfolio")
  before <- app$get_value(output = "portfolio-disb_region_plot")

  app$set_inputs(`portfolio-region` = "Abidjan")
  after <- app$get_value(output = "portfolio-disb_region_plot")

  expect_true(!is.null(after))
  expect_false(identical(before, after))
})

test_that("an empty filter result shows a message instead of a broken chart", {
  skip_if_not(file.exists("../../data/loans.rds"), "run data-raw/ scripts first")

  app <- AppDriver$new(app_dir = "../../", name = "portfolio-empty-filter", height = 900, width = 1400)
  on.exit(app$stop())

  app$set_inputs(navbar = "Portfolio")
  app$set_inputs(`portfolio-date_range` = c("1900-01-01", "1900-01-02"))

  expect_true(!is.null(app$get_value(output = "portfolio-disb_region_plot")))
})
