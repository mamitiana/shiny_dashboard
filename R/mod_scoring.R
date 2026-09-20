#' Scoring view - UI
#'
#' Applicant lookup, score/decision, per-variable contribution, and model
#' validation output, so performance stays visible next to the score itself.
#'
#' @param id Module namespace id.
#' @return A `shiny.tag.list`.
mod_scoring_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Applicant",
      shiny::selectizeInput(ns("loan_id"), "Select loan ID", choices = NULL),
      shiny::helpText("Data is synthetic; no real applicants are represented.")
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Score & decision"),
        shiny::uiOutput(ns("decision_box")),
        shiny::plotOutput(ns("contribution_plot"), height = "320px")
      ),
      bslib::card(
        bslib::card_header("Plain-language reading"),
        shiny::uiOutput(ns("explanation_text"))
      )
    ),
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      bslib::card(bslib::card_header("KS statistic"), shiny::uiOutput(ns("ks_value"))),
      bslib::card(bslib::card_header("Score distribution by outcome"),
                  shiny::plotOutput(ns("score_dist_plot"), height = "260px")),
      bslib::card(bslib::card_header("Discrimination by segment"),
                  DT::DTOutput(ns("segment_table")))
    )
  )
}

#' Scoring view - server
#'
#' @param id Module namespace id.
#' @param loans Reactive-free tibble of all loans, loaded once at startup.
#' @param model Fitted `glm` model object, loaded once at startup.
#' @param scale List with `slope`/`intercept` for score scaling, loaded once
#'   at startup.
#' @return `NULL`; this module does not expose reactives to the parent
#'   server (the two views are independent per the architecture rules).
mod_scoring_server <- function(id, loans, model, scale) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::updateSelectizeInput(
      session, "loan_id",
      choices = loans$loan_id,
      selected = loans$loan_id[[1]],
      server = TRUE
    )

    applicant <- shiny::reactive({
      shiny::req(input$loan_id)
      loans[loans$loan_id == input$loan_id, , drop = FALSE]
    }) |> shiny::bindEvent(input$loan_id)

    scored <- shiny::reactive({
      score_applicant(model, applicant(), scale)
    })

    contributions <- shiny::reactive({
      score_contributions(model, applicant(), scale)
    })

    band_stats <- shiny::reactive(score_distribution_by_outcome(loans))

    output$decision_box <- shiny::renderUI({
      s <- scored()
      decision <- dplyr::case_when(
        s$band %in% c("A", "B") ~ "Approve",
        s$band %in% c("D", "E") ~ "Decline",
        TRUE ~ "Refer"
      )
      shiny::tagList(
        shiny::h3(sprintf("Score: %.0f", s$score)),
        shiny::h4(sprintf("Band %s — %s", s$band, decision))
      )
    })

    output$contribution_plot <- shiny::renderPlot({
      c <- contributions()
      ggplot2::ggplot(c, ggplot2::aes(x = stats::reorder(.data$variable, .data$contribution),
                                        y = .data$contribution,
                                        fill = .data$contribution > 0)) +
        ggplot2::geom_col(show.legend = FALSE) +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "Contribution to score (points)") +
        ggplot2::theme_minimal()
    })

    output$explanation_text <- shiny::renderUI({
      s <- scored()
      bs <- band_stats()
      row <- bs[bs$band == s$band, , drop = FALSE]
      n_in_band <- if (nrow(row) == 1) row$n_loans else 0
      bad_rate <- if (nrow(row) == 1) row$bad_rate else NA_real_
      shiny::p(explain_score(s$band, bad_rate, n_in_band))
    })

    output$ks_value <- shiny::renderUI({
      ks <- ks_statistic(loans$score, loans$bad_flag)
      shiny::h3(sprintf("%.3f", ks$ks))
    })

    output$score_dist_plot <- shiny::renderPlot({
      bs <- band_stats()
      ggplot2::ggplot(bs, ggplot2::aes(x = .data$band, y = .data$bad_rate)) +
        ggplot2::geom_col(fill = "#1f6f5c") +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::labs(x = label_lookup("band"), y = "30+ DPD rate") +
        ggplot2::theme_minimal()
    })

    output$segment_table <- DT::renderDT({
      discrimination_by_segment(loans, "region") |>
        dplyr::mutate(
          ks = round(.data$ks, 3),
          auc = round(.data$auc, 3)
        )
    }, options = list(pageLength = 5), rownames = FALSE)

    invisible(NULL)
  })
}
