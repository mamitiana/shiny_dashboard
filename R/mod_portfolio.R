#' Portfolio view - UI
#'
#' Disbursement breakdowns, PAR trends, AML transaction-monitoring flags and
#' KYC completeness, all filterable by date range, branch/region and
#' product.
#'
#' @param id Module namespace id.
#' @return A `shiny.tag.list`.
mod_portfolio_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Filters",
      shiny::dateRangeInput(ns("date_range"), "Disbursement date range"),
      shiny::selectizeInput(ns("region"), "Region", choices = NULL, multiple = TRUE),
      shiny::selectizeInput(ns("product"), "Product", choices = NULL, multiple = TRUE),
      shiny::helpText("Data is synthetic; figures do not reflect any real portfolio.")
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header("Disbursement by region"),
                  shiny::plotOutput(ns("disb_region_plot"), height = "280px")),
      bslib::card(bslib::card_header("Disbursement by amount band"),
                  shiny::plotOutput(ns("disb_amount_plot"), height = "280px"))
    ),
    bslib::card(
      bslib::card_header("Portfolio-at-risk trend"),
      shiny::plotOutput(ns("par_trend_plot"), height = "280px")
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header("Transaction-monitoring flags"),
                  DT::DTOutput(ns("aml_table"))),
      bslib::card(bslib::card_header("KYC completeness by branch"),
                  DT::DTOutput(ns("kyc_table")))
    )
  )
}

#' Portfolio view - server
#'
#' @param id Module namespace id.
#' @param loans Tibble of all loans, loaded once at startup.
#' @param aml_flags List of `structuring`/`velocity`/`unusual_counterparty`
#'   tibbles precomputed by `data-raw/compute_flags.R`, loaded once at
#'   startup. Recomputing these per filter change is too slow at portfolio
#'   scale - see that script's header comment.
#' @return `NULL`.
mod_portfolio_server <- function(id, loans, aml_flags) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::updateDateRangeInput(
      session, "date_range",
      start = min(loans$disbursement_date), end = max(loans$disbursement_date),
      min = min(loans$disbursement_date), max = max(loans$disbursement_date)
    )
    shiny::updateSelectizeInput(session, "region", choices = sort(unique(loans$region)), server = TRUE)
    shiny::updateSelectizeInput(session, "product", choices = sort(unique(loans$product)), server = TRUE)

    # Single reactive filter feeding every downstream output, per the
    # "compute once, use many" reactivity rule.
    filtered_loans <- shiny::reactive({
      shiny::req(input$date_range)
      d <- loans[loans$disbursement_date >= input$date_range[1] &
                   loans$disbursement_date <= input$date_range[2], , drop = FALSE]
      if (length(input$region) > 0) d <- d[d$region %in% input$region, , drop = FALSE]
      if (length(input$product) > 0) d <- d[d$product %in% input$product, , drop = FALSE]
      d
    })

    empty_state <- function(plot_fn) {
      function() {
        if (nrow(filtered_loans()) == 0) {
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0, y = 0, label = "No loans match the current filters") +
            ggplot2::theme_void()
        } else {
          plot_fn()
        }
      }
    }

    output$disb_region_plot <- shiny::renderPlot({
      empty_state(function() {
        d <- disbursement_breakdown(filtered_loans(), "region")
        ggplot2::ggplot(d, ggplot2::aes(x = stats::reorder(.data$region, .data$total_amount), y = .data$total_amount)) +
          ggplot2::geom_col(fill = "#1f6f5c") +
          ggplot2::coord_flip() +
          ggplot2::scale_y_continuous(labels = scales::dollar) +
          ggplot2::labs(x = NULL, y = "Total disbursed") +
          ggplot2::theme_minimal()
      })()
    }) |> shiny::bindCache(input$date_range, input$region, input$product)

    output$disb_amount_plot <- shiny::renderPlot({
      empty_state(function() {
        d <- disbursement_breakdown(filtered_loans(), "amount_band")
        ggplot2::ggplot(d, ggplot2::aes(x = .data$amount_band, y = .data$n_loans)) +
          ggplot2::geom_col(fill = "#1f6f5c") +
          ggplot2::labs(x = label_lookup("amount_band"), y = "Number of loans") +
          ggplot2::theme_minimal()
      })()
    }) |> shiny::bindCache(input$date_range, input$region, input$product)

    output$par_trend_plot <- shiny::renderPlot({
      empty_state(function() {
        d <- par_trend(filtered_loans())
        d_long <- tidyr::pivot_longer(d, cols = c("par30", "par60", "par90"),
                                       names_to = "metric", values_to = "rate")
        ggplot2::ggplot(d_long, ggplot2::aes(x = .data$period, y = .data$rate, color = .data$metric)) +
          ggplot2::geom_line(linewidth = 1) +
          ggplot2::scale_y_continuous(labels = scales::percent) +
          ggplot2::labs(x = NULL, y = "Portfolio at risk", color = NULL) +
          ggplot2::theme_minimal()
      })()
    }) |> shiny::bindCache(input$date_range, input$region, input$product)

    # DT::renderDT() output is marked non-cacheable by Shiny, so bindCache()
    # (used on the plots above) isn't available here; filtering the
    # precomputed flags by loan_id is cheap regardless.
    output$aml_table <- DT::renderDT({
      ids <- filtered_loans()$loan_id
      dplyr::bind_rows(
        dplyr::mutate(aml_flags$structuring[aml_flags$structuring$loan_id %in% ids, , drop = FALSE],
                       rule = "Structuring", .before = 1),
        dplyr::mutate(aml_flags$velocity[aml_flags$velocity$loan_id %in% ids, , drop = FALSE],
                       rule = "Velocity", .before = 1),
        dplyr::mutate(aml_flags$unusual_counterparty[aml_flags$unusual_counterparty$loan_id %in% ids, , drop = FALSE],
                       rule = "Unusual counterparty", .before = 1)
      ) |>
        dplyr::select("rule", "loan_id", dplyr::everything())
    }, options = list(pageLength = 5), rownames = FALSE)

    output$kyc_table <- DT::renderDT({
      kyc_completeness(filtered_loans()) |>
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric) & !"n_loans", fmt_pct))
    }, options = list(pageLength = 5), rownames = FALSE)

    invisible(NULL)
  })
}
