#' Format a proportion as a percentage string
#'
#' Routes all percentage display through one place so rounding/decimals stay
#' consistent across scoring and portfolio views.
#'
#' @param x Numeric vector of proportions (0-1).
#' @param digits Number of decimal places. Default 1.
#' @return Character vector, e.g. "12.3%". `NA` for non-finite input.
fmt_pct <- function(x, digits = 1) {
  out <- ifelse(is.finite(x), sprintf(paste0("%.", digits, "f%%"), x * 100), NA_character_)
  out
}

#' Format a monetary amount
#'
#' @param x Numeric vector of amounts.
#' @param currency_symbol Symbol prefix. Default "$".
#' @return Character vector, e.g. "$12,345".
fmt_currency <- function(x, currency_symbol = "$") {
  ifelse(
    is.finite(x),
    paste0(currency_symbol, formatC(round(x), format = "d", big.mark = ",")),
    NA_character_
  )
}

#' Format a plain count with thousands separators
#'
#' @param x Numeric vector.
#' @return Character vector, e.g. "12,345".
fmt_number <- function(x) {
  ifelse(is.finite(x), formatC(x, format = "d", big.mark = ","), NA_character_)
}

#' Format a date for display
#'
#' @param x Date vector.
#' @param fmt strftime format. Default "%b %Y".
#' @return Character vector.
fmt_date <- function(x, fmt = "%b %Y") {
  format(x, fmt)
}

#' Human-readable labels for internal column/segment names
#'
#' Keeps display wording out of modules and plotting code so a wording change
#' happens in one place.
#'
#' @param key Character vector of internal names (e.g. "region", "par30").
#' @return Character vector of display labels; unknown keys are returned
#'   title-cased unchanged rather than erroring, since new segment types are
#'   expected to appear before their label is registered.
label_lookup <- function(key) {
  known <- c(
    age            = "Age",
    sex            = "Sex",
    region         = "Region",
    branch         = "Branch",
    product        = "Product",
    amount         = "Disbursed amount",
    amount_band    = "Amount band",
    score          = "Score",
    band           = "Score band",
    par30          = "PAR 30",
    par60          = "PAR 60",
    par90          = "PAR 90",
    bad_flag       = "30+ DPD ever",
    dpd            = "Days past due",
    kyc_complete   = "KYC complete"
  )
  out <- unname(known[key])
  missing <- is.na(out)
  if (any(missing)) {
    # tools::toTitleCase(character(0)) returns list(), not character(0), so
    # this must be skipped when nothing is missing or it coerces `out` to a
    # list via the replacement assignment below. It also leaves a leading
    # lowercase letter untouched (e.g. "some new field" -> "some New
    # Field"), hence the explicit substr() fix-up on the first character.
    titled <- tools::toTitleCase(gsub("_", " ", key[missing]))
    substr(titled, 1, 1) <- toupper(substr(titled, 1, 1))
    out[missing] <- titled
  }
  out
}

#' Report "n too small" instead of a rate computed on a tiny sample
#'
#' Encodes the app's honesty-about-uncertainty rule: a rate on too few
#' observations reads as more precise than it is.
#'
#' @param n Integer, the sample size backing a rate.
#' @param min_n Minimum sample size required to report. Default 30.
#' @return TRUE if n is below the reporting threshold.
n_too_small <- function(n, min_n = 30) {
  is.na(n) | n < min_n
}
