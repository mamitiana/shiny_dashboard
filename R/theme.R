#' App theme
#'
#' Single bslib theme object used across the whole app. Defined once here so
#' colors/fonts change in one place instead of drifting between modules.
#'
#' @return A `bslib::bs_theme()` object.
app_theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#1f6f5c",
  base_font = bslib::font_google("Inter"),
  heading_font = bslib::font_google("Inter")
)
