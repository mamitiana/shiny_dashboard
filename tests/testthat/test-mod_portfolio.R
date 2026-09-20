# shiny::testServer() exercises the module's reactive graph directly,
# without a browser - see test-mod_scoring.R for why this exists
# alongside the fct_* unit tests.

test_that("mod_portfolio_server renders every output for a normal filter", {
  skip_if_not(file.exists("../../data/loans.rds"), "run data-raw/ scripts first")
  skip_if_not(file.exists("../../data/aml_flags.rds"), "run data-raw/compute_flags.R first")

  loans <- readRDS("../../data/loans.rds")
  aml_flags <- readRDS("../../data/aml_flags.rds")

  shiny::testServer(
    mod_portfolio_server,
    args = list(loans = loans, aml_flags = aml_flags),
    {
      session$setInputs(
        date_range = c(min(loans$disbursement_date), max(loans$disbursement_date)),
        region = character(0),
        product = character(0)
      )

      expect_true(!is.null(output$disb_region_plot))
      expect_true(!is.null(output$disb_amount_plot))
      expect_true(!is.null(output$par_trend_plot))
      expect_true(!is.null(output$aml_table))
      expect_true(!is.null(output$kyc_table))
    }
  )
})

test_that("mod_portfolio_server renders a message instead of erroring on an empty filter", {
  skip_if_not(file.exists("../../data/loans.rds"), "run data-raw/ scripts first")
  skip_if_not(file.exists("../../data/aml_flags.rds"), "run data-raw/compute_flags.R first")

  loans <- readRDS("../../data/loans.rds")
  aml_flags <- readRDS("../../data/aml_flags.rds")

  shiny::testServer(
    mod_portfolio_server,
    args = list(loans = loans, aml_flags = aml_flags),
    {
      session$setInputs(
        date_range = as.Date(c("1900-01-01", "1900-01-02")),
        region = character(0),
        product = character(0)
      )

      expect_true(!is.null(output$disb_region_plot))
      expect_true(!is.null(output$par_trend_plot))
    }
  )
})
