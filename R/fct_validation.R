#' Kolmogorov-Smirnov statistic for a binary classifier
#'
#' KS is the maximum gap between the cumulative distributions of scores for
#' the good and bad populations — computed here from empirical CDFs so no
#' extra modeling package is required beyond base R.
#'
#' @param score Numeric vector of scores.
#' @param bad_flag Logical or 0/1 vector, `TRUE`/1 = went 30+ DPD.
#' @return A list: `ks` (the statistic, 0-1), `score_at_ks` (the score where
#'   the max gap occurs). `NA` for both if either class is empty.
ks_statistic <- function(score, bad_flag) {
  bad_flag <- as.logical(bad_flag)
  good <- score[!bad_flag]
  bad <- score[bad_flag]
  if (length(good) == 0 || length(bad) == 0) {
    return(list(ks = NA_real_, score_at_ks = NA_real_))
  }
  grid <- sort(unique(score))
  cdf_good <- stats::ecdf(good)(grid)
  cdf_bad <- stats::ecdf(bad)(grid)
  gap <- abs(cdf_good - cdf_bad)
  list(ks = max(gap), score_at_ks = grid[which.max(gap)])
}

#' AUC via the Mann-Whitney U statistic
#'
#' AUC equals the probability that a randomly chosen bad case scores lower
#' (riskier) than a randomly chosen good case; computed from rank sums so no
#' ROC-curve package dependency is needed.
#'
#' @param score Numeric vector of scores (higher = lower risk).
#' @param bad_flag Logical or 0/1 vector, `TRUE`/1 = went 30+ DPD.
#' @return Numeric AUC in \[0, 1\], or `NA` if either class is empty.
roc_auc <- function(score, bad_flag) {
  bad_flag <- as.logical(bad_flag)
  # as.double(): n_bad * n_good overflows the 32-bit integer range on a
  # portfolio of a few hundred thousand loans (sum() of a logical vector
  # returns integer).
  n_good <- as.double(sum(!bad_flag))
  n_bad <- as.double(sum(bad_flag))
  if (n_good == 0 || n_bad == 0) return(NA_real_)
  r <- rank(score)
  sum_rank_bad <- sum(r[bad_flag])
  u_bad <- sum_rank_bad - n_bad * (n_bad + 1) / 2
  # AUC framed as P(good scores higher than bad); u_bad/(n_bad*n_good) is
  # P(bad ranks higher), so this is its complement.
  1 - u_bad / (n_bad * n_good)
}

#' Score distribution by outcome class, with per-band bad rates
#'
#' Backs both the score-distribution chart and the plain-language band
#' reading in the scoring view.
#'
#' @param loans Data frame with `score` and `bad_flag` columns.
#' @param bands Score band table, `PORTFOLIO_CONFIG$score_bands` by default.
#' @return Tibble: `band`, `n_loans`, `bad_rate`, `too_small` (logical, per
#'   `n_too_small()`).
score_distribution_by_outcome <- function(loans, bands = PORTFOLIO_CONFIG$score_bands) {
  if (nrow(loans) == 0) {
    return(tibble::tibble(band = character(0), n_loans = integer(0),
                           bad_rate = double(0), too_small = logical(0)))
  }
  loans |>
    dplyr::mutate(band = score_to_band(.data$score, bands)) |>
    dplyr::group_by(.data$band) |>
    dplyr::summarise(
      n_loans = dplyr::n(),
      bad_rate = mean(.data$bad_flag, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(too_small = n_too_small(.data$n_loans, PORTFOLIO_CONFIG$min_n_for_rate))
}

#' Model discrimination (KS, AUC) by segment
#'
#' Lets an officer see whether the model discriminates as well within a
#' subgroup (e.g. region, product) as it does overall — segments with too
#' few loans are marked rather than given a falsely precise AUC.
#'
#' @param loans Data frame with `score`, `bad_flag`, and `segment_var`.
#' @param segment_var String, column name to segment by.
#' @param min_n Minimum loans required to report a segment's metrics.
#'   Default `PORTFOLIO_CONFIG$min_n_for_rate`.
#' @return Tibble: the segment column, `n_loans`, `ks`, `auc`, `too_small`.
discrimination_by_segment <- function(loans, segment_var, min_n = PORTFOLIO_CONFIG$min_n_for_rate) {
  if (nrow(loans) == 0) {
    return(tibble::tibble(
      !!segment_var := character(0), n_loans = integer(0),
      ks = double(0), auc = double(0), too_small = logical(0)
    ))
  }
  loans |>
    dplyr::group_by(dplyr::across(dplyr::all_of(segment_var))) |>
    dplyr::group_modify(~ {
      n <- nrow(.x)
      if (n_too_small(n, min_n)) {
        return(tibble::tibble(n_loans = n, ks = NA_real_, auc = NA_real_, too_small = TRUE))
      }
      tibble::tibble(
        n_loans = n,
        ks = ks_statistic(.x$score, .x$bad_flag)$ks,
        auc = roc_auc(.x$score, .x$bad_flag),
        too_small = FALSE
      )
    }) |>
    dplyr::ungroup()
}
