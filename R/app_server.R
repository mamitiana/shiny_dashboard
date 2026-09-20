#' Top-level app server
#'
#' Wires the two view modules to the data/model loaded once at startup.
#' Modules never reach into each other's inputs; each is self-contained.
#'
#' @param input,output,session Standard Shiny server arguments.
#' @param loans Tibble of all loans, loaded once in `app.R`.
#' @param model Fitted `glm` scoring model, loaded once in `app.R`.
#' @param scale List with `slope`/`intercept` for score scaling, loaded once
#'   in `app.R`.
#' @param aml_flags Precomputed AML flags list, loaded once in `app.R` (see
#'   `data-raw/compute_flags.R`).
app_server <- function(input, output, session, loans, model, scale, aml_flags) {
  mod_scoring_server("scoring", loans, model, scale)
  mod_portfolio_server("portfolio", loans, aml_flags)
}
