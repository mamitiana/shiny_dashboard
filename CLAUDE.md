# CLAUDE.md — Loan Scoring & Portfolio Shiny App (public demo)

## What this project is

A public, shareable R Shiny application that mirrors a private credit-scoring and
loan-portfolio tool I built on client data. This version runs entirely on **synthetic
data generated in-repo** — no client data, ever.

It exists to be a portfolio artifact: a link I can put on a job application that shows
I can build, structure, test and deploy a Shiny app for non-technical users.

## Who the user is

A loan officer or portfolio manager with no statistical training. They need to make a
decision, not read a model report. Every design choice follows from that:

- Clear defaults over exhaustive options.
- Show uncertainty honestly (confidence bands, sample sizes, "n too small to report").
- Never display a number without the denominator or period it refers to.
- If a chart needs a paragraph of explanation, it is the wrong chart.

## Scope

**Two modules. Do not add a third without being asked.**

### 1. Scoring view
- Select or enter an applicant.
- Show: model score, approve/decline/refer outcome, and the **variable-level
  contribution** behind that score (waterfall or bar of per-feature contributions).
- Beside it, model-validation output so performance stays visible to the people relying
  on the score: KS statistic, score distribution by outcome class, ROC/AUC,
  discrimination by segment.
- Plain-language reading of the score for the officer ("this applicant scores in band C;
  historically 12% of band C loans went 30+ days past due").

### 2. Portfolio view
- Disbursement broken down by age, sex, location, amount band.
- Portfolio-at-risk (PAR 30 / 60 / 90) trends over time.
- Transaction-monitoring flags (AML/CFT-style rules: structuring, velocity, unusual
  counterparties).
- KYC completeness by field and by branch.
- Every breakdown filterable by date range, branch/region, and product.

## Non-goals

- No authentication/user management (it is a public demo).
- No real or client data. No re-identifiable data. Synthetic only.
- No model training pipeline in the app — the model is fit offline in
  `data-raw/`, serialized, and loaded read-only.
- No dashboard-builder generality. Two fixed views, done well.

## Tech stack

- **R** (>= 4.3), **Shiny** with `moduleServer()` modules — one module per view, plus
  shared UI helpers.
- **bslib** for theming/layout. One theme object, defined once in `R/theme.R`.
- **ggplot2** for static charts, **DT** for tables. Add `plotly` only where interaction
  earns its cost.
- **tidyverse** / **data.table** for data prep.
- **renv** for dependency locking — commit `renv.lock`.
- **testthat** for unit tests, **shinytest2** for app-level tests.
- Deployment target: **shinyapps.io** (free tier). Keep the app under its memory limit.

## Repository layout

```
.
├── app.R                  # thin: loads R/, calls app_ui() / app_server()
├── R/
│   ├── app_ui.R
│   ├── app_server.R
│   ├── mod_scoring.R      # scoring view module (UI + server)
│   ├── mod_portfolio.R    # portfolio view module (UI + server)
│   ├── fct_scoring.R      # scoring + explanation logic, no Shiny code
│   ├── fct_portfolio.R    # PAR, KYC, AML metric functions, no Shiny code
│   ├── fct_validation.R   # KS, ROC/AUC, segment discrimination
│   ├── utils_format.R     # number/date/label formatting
│   └── theme.R
├── data-raw/
│   ├── generate_synthetic.R   # makes the dataset; seeded and reproducible
│   └── fit_model.R            # fits + serializes the scoring model
├── data/                  # generated artifacts (parquet/rds), gitignored if large
├── tests/
│   ├── testthat/          # unit tests for fct_* functions
│   └── testthat/test-app.R # shinytest2 snapshots
├── renv.lock
└── README.md
```

## Architecture rules

1. **Business logic lives outside Shiny.** Every `fct_*.R` function takes plain data
   frames and arguments and returns data frames or plots. They must be callable and
   testable with no Shiny session. Modules only wire inputs to these functions.
2. **One module per view**, namespaced with `NS(id)`. Modules never reach into each
   other's inputs; they communicate by returning reactives to the parent server.
3. **Reactivity discipline.** Compute once, use many: expensive filtering goes in a
   single `reactive()` that downstream outputs consume. Use `bindCache()` for anything
   recomputed on the same inputs and `bindEvent()` where a control should not fire on
   every keystroke. No `observe()` writing to global state.
4. **Data is loaded once at startup**, outside the server function, and treated as
   immutable.
5. **No hardcoded strings in plots or labels** — route through `utils_format.R` so
   wording changes happen in one place.

## Performance targets

- Cold start under 5 seconds.
- Any filter change repaints in under 1 second at 250k rows.
- If a view cannot hit that, pre-aggregate in `data-raw/` rather than optimizing the
  reactive graph indefinitely.

## Data

`data-raw/generate_synthetic.R` produces a loan book of roughly 250k rows with:
borrower age, sex, region, branch, product, disbursement date and amount, repayment
history, KYC field completeness, and transaction records. It must be **seeded** so the
dataset is reproducible, and realistic enough that PAR and score distributions are not
uniform — build in genuine segment differences so the charts have something to show.

State clearly in the app UI that the data is synthetic. Do not let a reader mistake
demo output for real portfolio performance.

## Coding conventions

- Follow the tidyverse style guide. Snake_case throughout.
- Every exported function gets a roxygen block: what it takes, what it returns, and one
  sentence on why it exists.
- Functions do one thing. If a function needs a section comment to explain its second
  half, split it.
- No magic numbers. PAR thresholds, score bands and AML rule parameters live in a single
  config list in `R/fct_portfolio.R`.
- Comment the *why*, not the *what*.

## Testing

- Unit tests for every `fct_*` function, including edge cases: empty filter result,
  single-row group, all-missing column, division by zero in a rate.
- `shinytest2` coverage for: app loads, each view renders, a filter change updates
  output, and an empty-result state shows a message rather than a broken chart.
- Run tests before every commit. A failing test blocks the commit.

## Commands

```r
renv::restore()                          # set up dependencies
source("data-raw/generate_synthetic.R")  # build the dataset
source("data-raw/fit_model.R")           # fit and serialize the model
shiny::runApp()                          # run locally
testthat::test_dir("tests/testthat")     # unit tests
shinytest2::test_app()                   # app tests
rsconnect::deployApp()                   # deploy to shinyapps.io
```

## Working agreements for Claude

- Ask before adding a dependency. The stack above is deliberate and small.
- Ask before adding a feature not in Scope. Scope creep is the main risk to this project.
- When changing a module, run the tests for that module and report the result — do not
  report a change as done on the basis of the code reading correctly.
- Prefer editing an existing file over creating a new one.
- Keep commits small and focused. One logical change per commit, message in the
  imperative ("add PAR trend chart", not "added charts and fixed stuff").
- When a design decision has a real trade-off (performance vs. clarity, more detail vs.
  less), say so and let me choose rather than picking silently.

## Definition of done

- Deployed and reachable at a public URL.
- README with a screenshot, a one-paragraph description, and the live link.
- All tests passing; `renv.lock` committed.
- A stranger can open the URL and understand what the app is for within 15 seconds.