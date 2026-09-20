# data-raw/fit_model.R
#
# Fits the scoring model on the synthetic loan book and serializes it,
# read-only, for the Shiny app. The app itself never trains anything.
#
# Must be run after generate_synthetic.R. Run from the project root:
#   source("data-raw/fit_model.R")

source("R/fct_portfolio.R")  # PORTFOLIO_CONFIG, score_to_band()
source("R/fct_validation.R") # roc_auc()

loans <- readRDS("data/loans.rds")

set.seed(20240115)
train_idx <- sample.int(nrow(loans), size = round(0.7 * nrow(loans)))
train <- loans[train_idx, ]

model <- stats::glm(
  bad_flag ~ age + amount + region + product + sex,
  data = train,
  family = stats::binomial()
)

# Map the linear predictor onto a 300-850 display range. Higher linear
# predictor means higher default risk, so the slope is negative: higher
# score should mean lower risk, matching PORTFOLIO_CONFIG$score_bands.
lp <- stats::predict(model, newdata = loans, type = "link")
lp_range <- range(lp)
slope <- -(850 - 300) / diff(lp_range)
intercept <- 850 - slope * lp_range[1]
scale <- list(slope = slope, intercept = intercept)

loans$score <- pmin(pmax(intercept + slope * lp, 300), 850)
loans$band <- score_to_band(loans$score)

saveRDS(loans, "data/loans.rds")
saveRDS(list(model = model, scale = scale), "data/model.rds")

message(sprintf(
  "Fit model on %s training loans; overall AUC = %.3f, KS = %.3f",
  nrow(train),
  roc_auc(loans$score, loans$bad_flag),
  ks_statistic(loans$score, loans$bad_flag)$ks
))
