# shiny::testServer() exercises the module's reactive graph directly,
# without a browser - it is what catches wiring bugs (missing inputs,
# renderPlot/renderUI errors) that the fct_* unit tests cannot see.

test_that("mod_scoring_server renders every output without error", {
  skip_if_not(file.exists("../../data/loans.rds"), "run data-raw/ scripts first")

  loans <- readRDS("../../data/loans.rds")
  bundle <- readRDS("../../data/model.rds")

  shiny::testServer(
    mod_scoring_server,
    args = list(loans = loans, model = bundle$model, scale = bundle$scale),
    {
      session$setInputs(loan_id = loans$loan_id[[1]])

      expect_true(!is.null(output$decision_box))
      expect_true(!is.null(output$explanation_text))
      expect_true(!is.null(output$contribution_plot))
      expect_true(!is.null(output$ks_value))
      expect_true(!is.null(output$score_dist_plot))
      expect_true(!is.null(output$segment_table))
    }
  )
})
