#' Score a single applicant
#'
#' Wraps `predict.glm()` so the rest of the app never touches the model
#' object directly — if the model type changes later, only this function
#' needs to change.
#'
#' @param model A fitted `glm` object (binomial, logit link) as produced by
#'   `data-raw/fit_model.R`.
#' @param applicant Single-row data frame with the model's predictor
#'   columns.
#' @param scale List with `slope`/`intercept` used to map the linear
#'   predictor onto the 300-850 display range, as saved alongside the model.
#' @return A one-row tibble: `score`, `band`, `pd` (predicted probability of
#'   default).
score_applicant <- function(model, applicant, scale) {
  stopifnot(nrow(applicant) == 1)
  lp <- predict(model, newdata = applicant, type = "link")
  score <- scale$intercept + scale$slope * lp
  score <- pmin(pmax(score, 300), 850)
  tibble::tibble(
    score = as.numeric(score),
    band = score_to_band(as.numeric(score)),
    pd = as.numeric(predict(model, newdata = applicant, type = "response"))
  )
}

#' Per-variable contribution to a single applicant's score
#'
#' Uses `predict(..., type = "terms")`, which for a GLM returns each term's
#' centered contribution to the linear predictor — the correct basis for a
#' waterfall chart on a linear/logit model, as opposed to an ad hoc
#' sensitivity analysis.
#'
#' @param model A fitted `glm` object.
#' @param applicant Single-row data frame with the model's predictor
#'   columns.
#' @param scale List with `slope` used to convert linear-predictor units
#'   into score points, matching `score_applicant()`.
#' @return Tibble with one row per model term: `variable`, `contribution`
#'   (in score points), ordered by descending absolute contribution.
score_contributions <- function(model, applicant, scale) {
  stopifnot(nrow(applicant) == 1)
  terms_mat <- predict(model, newdata = applicant, type = "terms")
  tibble::tibble(
    variable = colnames(terms_mat),
    contribution = as.numeric(terms_mat[1, ]) * scale$slope
  ) |>
    dplyr::arrange(dplyr::desc(abs(.data$contribution)))
}

#' Plain-language reading of a score band for a loan officer
#'
#' @param band Character, the applicant's score band (e.g. "C").
#' @param historical_bad_rate Named numeric vector or tibble giving the
#'   historical 30+ DPD rate per band, as returned by
#'   `score_distribution_by_outcome()`; looked up by `band`.
#' @param n_in_band Sample size backing that historical rate, for the
#'   "n too small to report" guard.
#' @return A single character string, e.g. "This applicant scores in band C;
#'   historically 12.0% of band C loans went 30+ days past due (based on
#'   1,204 loans)."
explain_score <- function(band, historical_bad_rate, n_in_band) {
  if (is.na(band)) {
    return("This applicant's score falls outside the defined bands and cannot be read against historical performance.")
  }
  if (n_too_small(n_in_band, PORTFOLIO_CONFIG$min_n_for_rate)) {
    return(sprintf(
      "This applicant scores in band %s; too few historical loans in this band (%s) to report a reliable default rate.",
      band, fmt_number(n_in_band)
    ))
  }
  sprintf(
    "This applicant scores in band %s; historically %s of band %s loans went 30+ days past due (based on %s loans).",
    band, fmt_pct(historical_bad_rate), band, fmt_number(n_in_band)
  )
}
