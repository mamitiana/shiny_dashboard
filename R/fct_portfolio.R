#' Portfolio metric configuration
#'
#' Single source of truth for PAR thresholds, score bands and AML rule
#' parameters, so none of them are hardcoded inside the metric functions
#' below.
PORTFOLIO_CONFIG <- list(
  par_days = c(par30 = 30, par60 = 60, par90 = 90),
  score_bands = tibble::tribble(
    ~band, ~min_score, ~max_score,
    "A",   720,        850,
    "B",   660,        719,
    "C",   600,        659,
    "D",   500,        599,
    "E",   300,        499
  ),
  aml = list(
    structuring_threshold_amount = 10000,
    structuring_window_days      = 3,
    structuring_min_txns         = 3,
    velocity_txn_count           = 10,
    velocity_window_days         = 1,
    unusual_counterparty_zscore  = 3
  ),
  min_n_for_rate = 30
)

#' Assign a score band
#'
#' @param score Numeric vector of model scores.
#' @param bands Score band table, `PORTFOLIO_CONFIG$score_bands` by default.
#' @return Character vector of band labels; `NA` where score falls outside
#'   all defined bands.
score_to_band <- function(score, bands = PORTFOLIO_CONFIG$score_bands) {
  idx <- vapply(score, function(s) {
    if (is.na(s)) return(NA_integer_)
    hit <- which(s >= bands$min_score & s <= bands$max_score)
    if (length(hit) == 0) NA_integer_ else hit[1]
  }, integer(1))
  bands$band[idx]
}

#' Portfolio-at-risk summary as of a reference date
#'
#' PAR-x is the share of outstanding principal on loans with days-past-due
#' greater than or equal to x, out of total outstanding principal — the
#' standard microfinance risk metric.
#'
#' @param loans Data frame with `outstanding_balance` and `dpd` (days past
#'   due) columns.
#' @param as_of_date Unused placeholder for point-in-time snapshots once the
#'   loan table carries a balance history; kept explicit in the signature so
#'   callers state their intent even though today it scores the table as-is.
#' @return A one-row tibble with par30, par60, par90 (proportions) and
#'   `n_loans`, `total_outstanding` as the denominator context.
par_summary <- function(loans, as_of_date = Sys.Date()) {
  total_outstanding <- sum(loans$outstanding_balance, na.rm = TRUE)
  par <- vapply(PORTFOLIO_CONFIG$par_days, function(d) {
    if (total_outstanding <= 0) return(NA_real_)
    sum(loans$outstanding_balance[loans$dpd >= d], na.rm = TRUE) / total_outstanding
  }, numeric(1))
  tibble::tibble(
    par30 = par[["par30"]],
    par60 = par[["par60"]],
    par90 = par[["par90"]],
    n_loans = nrow(loans),
    total_outstanding = total_outstanding
  )
}

#' PAR trend over time
#'
#' @param loans Data frame with `disbursement_date`, `outstanding_balance`,
#'   `dpd`.
#' @param date_breaks Grouping period passed to `lubridate::floor_date()`
#'   (e.g. "month", "week"). Default "month".
#' @return Tibble with one row per period: `period`, par30/60/90, `n_loans`.
#'   Empty input returns a zero-row tibble with the same columns rather than
#'   erroring, so an empty-filter state can render a message instead of a
#'   broken chart.
par_trend <- function(loans, date_breaks = "month") {
  cols <- c("period", "par30", "par60", "par90", "n_loans", "total_outstanding")
  if (nrow(loans) == 0) {
    return(tibble::as_tibble(stats::setNames(rep(list(logical(0)), length(cols)), cols)))
  }
  loans |>
    dplyr::mutate(period = lubridate::floor_date(.data$disbursement_date, date_breaks)) |>
    dplyr::group_by(.data$period) |>
    dplyr::group_modify(~ par_summary(.x)) |>
    dplyr::ungroup()
}

#' Disbursement breakdown by a grouping variable
#'
#' @param loans Data frame including `amount` and the grouping column.
#' @param group_var String, name of the column to group by (e.g. "region",
#'   "sex", "amount_band").
#' @return Tibble with the group column, `n_loans`, `total_amount`,
#'   `share_of_loans`, `share_of_amount`.
disbursement_breakdown <- function(loans, group_var) {
  if (nrow(loans) == 0) {
    return(tibble::tibble(
      !!group_var := character(0), n_loans = integer(0), total_amount = double(0),
      share_of_loans = double(0), share_of_amount = double(0)
    ))
  }
  total_n <- nrow(loans)
  total_amt <- sum(loans$amount, na.rm = TRUE)
  loans |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_var))) |>
    dplyr::summarise(
      n_loans = dplyr::n(),
      total_amount = sum(.data$amount, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      share_of_loans = .data$n_loans / total_n,
      share_of_amount = ifelse(total_amt > 0, .data$total_amount / total_amt, NA_real_)
    )
}

#' KYC field completeness by branch
#'
#' @param loans Data frame with logical KYC field columns (see
#'   `kyc_fields`) and a `branch` column.
#' @param kyc_fields Character vector of column names to check. Defaults to
#'   the fields generated in `data-raw/generate_synthetic.R`.
#' @return Tibble: `branch`, one completeness-rate column per KYC field, and
#'   `n_loans` as the denominator.
kyc_completeness <- function(loans,
                              kyc_fields = c("kyc_id_doc", "kyc_proof_address",
                                             "kyc_phone_verified", "kyc_income_verified",
                                             "kyc_next_of_kin")) {
  if (nrow(loans) == 0) {
    empty <- stats::setNames(rep(list(double(0)), length(kyc_fields)), kyc_fields)
    return(tibble::tibble(branch = character(0), !!!empty, n_loans = integer(0)))
  }
  loans |>
    dplyr::group_by(.data$branch) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(kyc_fields), ~ mean(.x, na.rm = TRUE)),
      n_loans = dplyr::n(),
      .groups = "drop"
    )
}

#' Flag structuring-pattern transactions
#'
#' Structuring: multiple transactions on the same loan, each individually
#' under the reporting threshold, that sum above it within a short window —
#' a classic AML/CFT red flag for deliberate threshold avoidance.
#'
#' @param transactions Data frame with `loan_id`, `txn_date`, `amount`.
#' @param config AML parameter list, `PORTFOLIO_CONFIG$aml` by default.
#' @return Tibble of one row per flagged loan: `loan_id`, `n_txns`,
#'   `total_amount`, `window_start`, `window_end`.
flag_structuring <- function(transactions, config = PORTFOLIO_CONFIG$aml) {
  if (nrow(transactions) == 0) {
    return(tibble::tibble(loan_id = character(0), n_txns = integer(0),
                           total_amount = double(0), window_start = as.Date(character(0)),
                           window_end = as.Date(character(0))))
  }
  sub_threshold <- transactions |>
    dplyr::filter(.data$amount < config$structuring_threshold_amount)

  sub_threshold |>
    dplyr::arrange(.data$loan_id, .data$txn_date) |>
    dplyr::group_by(.data$loan_id) |>
    dplyr::group_modify(~ {
      d <- .x
      n <- nrow(d)
      if (n < config$structuring_min_txns) return(tibble::tibble())
      # Slide a window_days-wide window across this loan's sub-threshold
      # transactions. Dates are sorted, so for each window start `i` the
      # last transaction still inside the window is found with a binary
      # search (findInterval) instead of rescanning the whole group - the
      # original row-by-row purrr::map_dfr() built one tibble per
      # transaction, which is what made this take minutes at 250k-loan
      # scale (see data-raw/compute_flags.R for why this now runs offline
      # anyway).
      window_end <- as.numeric(d$txn_date) + config$structuring_window_days
      dt <- as.numeric(d$txn_date)
      j_max <- findInterval(window_end, dt)
      cnt <- j_max - seq_len(n) + 1
      cum_amt <- c(0, cumsum(d$amount))
      tot <- cum_amt[j_max + 1] - cum_amt[seq_len(n)]
      hit <- which(cnt >= config$structuring_min_txns & tot >= config$structuring_threshold_amount)
      if (length(hit) == 0) return(tibble::tibble())
      best <- hit[which.max(cnt[hit])]
      tibble::tibble(
        n_txns = cnt[best],
        total_amount = tot[best],
        window_start = d$txn_date[best],
        window_end = d$txn_date[j_max[best]]
      )
    }) |>
    dplyr::ungroup()
}

#' Flag unusually high transaction velocity
#'
#' Velocity: more transactions on a loan within a short window than
#' expected, which can indicate account takeover or layering.
#'
#' @param transactions Data frame with `loan_id`, `txn_date`.
#' @param config AML parameter list, `PORTFOLIO_CONFIG$aml` by default.
#' @return Tibble: `loan_id`, `window_start`, `n_txns`.
flag_velocity <- function(transactions, config = PORTFOLIO_CONFIG$aml) {
  if (nrow(transactions) == 0) {
    return(tibble::tibble(loan_id = character(0), window_start = as.Date(character(0)),
                           n_txns = integer(0)))
  }
  transactions |>
    dplyr::arrange(.data$loan_id, .data$txn_date) |>
    dplyr::mutate(day = as.Date(.data$txn_date)) |>
    dplyr::group_by(.data$loan_id, .data$day) |>
    dplyr::summarise(n_txns = dplyr::n(), .groups = "drop") |>
    dplyr::filter(.data$n_txns >= config$velocity_txn_count) |>
    dplyr::transmute(.data$loan_id, window_start = .data$day, .data$n_txns)
}

#' Flag transactions with unusual counterparties
#'
#' A counterparty is unusual for a loan if its transaction amount is a
#' statistical outlier relative to that loan's own transaction history
#' (z-score beyond the configured threshold). Requires at least 2
#' transactions to compute a standard deviation.
#'
#' @param transactions Data frame with `loan_id`, `counterparty_id`,
#'   `amount`.
#' @param config AML parameter list, `PORTFOLIO_CONFIG$aml` by default.
#' @return Tibble: `loan_id`, `counterparty_id`, `amount`, `zscore`.
flag_unusual_counterparty <- function(transactions, config = PORTFOLIO_CONFIG$aml) {
  if (nrow(transactions) == 0) {
    return(tibble::tibble(loan_id = character(0), counterparty_id = character(0),
                           amount = double(0), zscore = double(0)))
  }
  transactions |>
    dplyr::group_by(.data$loan_id) |>
    dplyr::filter(dplyr::n() >= 2) |>
    dplyr::mutate(
      # sd() is a single per-group value, so a vectorized ifelse()/if_else()
      # either collapses every row to one recycled value or errors on the
      # size mismatch - a plain scalar `if` avoids both.
      zscore = {
        sd_amt <- stats::sd(.data$amount)
        if (sd_amt > 0) (.data$amount - mean(.data$amount)) / sd_amt else rep(0, dplyr::n())
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(abs(.data$zscore) >= config$unusual_counterparty_zscore) |>
    dplyr::select("loan_id", "counterparty_id", "amount", "zscore")
}
