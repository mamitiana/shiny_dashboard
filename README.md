# Loan Scoring & Portfolio (demo)

A public Shiny app that shows a loan officer or portfolio manager a credit
score with its variable-level explanation, the model's validation stats, and
a portfolio-risk view (disbursement mix, PAR trends, AML-style transaction
flags, KYC completeness) — all on **synthetic data generated in this repo**.
It's a portfolio artifact mirroring a private tool built on client data; no
real or client data is used anywhere here.

**Live app:** _not yet deployed_
**Screenshot:** _to add once the data is generated and the app runs
end-to-end_

## Running it locally

```r
renv::restore()                          # install dependencies
source("data-raw/generate_synthetic.R")  # build the ~250k-row synthetic loan book
source("data-raw/fit_model.R")           # fit and serialize the scoring model
source("data-raw/compute_flags.R")       # precompute AML flags (too slow to run per-filter live)
shiny::runApp()                          # run the app
```

## Testing

```r
testthat::test_dir("tests/testthat")     # unit tests for R/fct_*.R
shinytest2::test_app()                   # app-level tests (needs data/*.rds)
```

## Project structure

See [CLAUDE.md](CLAUDE.md) for the full scope, architecture rules and
repository layout this project follows.
