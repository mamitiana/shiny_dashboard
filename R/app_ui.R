#' Top-level app UI
#'
#' Two fixed views (Scoring, Portfolio) in a navbar, plus a synthetic-data
#' notice so a reader never mistakes demo output for real performance.
#'
#' @return A `bslib::page_navbar()` UI definition.
app_ui <- function() {
  bslib::page_navbar(
    title = "Loan Scoring & Portfolio (demo)",
    id = "navbar",
    theme = app_theme,
    footer = shiny::div(
      class = "text-muted small p-2",
      "All data on this page is synthetically generated and does not represent any real applicant, borrower or institution."
    ),
    bslib::nav_panel("Scoring", mod_scoring_ui("scoring")),
    bslib::nav_panel("Portfolio", mod_portfolio_ui("portfolio"))
  )
}
