# Thin entry point: sources R/, loads data once, wires ui/server together.
# All logic lives in R/ so it can be tested outside a Shiny session.

purrr::walk(list.files("R", pattern = "\\.R$", full.names = TRUE), source)

loans <- readRDS("data/loans.rds")
model_bundle <- readRDS("data/model.rds")
aml_flags <- readRDS("data/aml_flags.rds")

shiny::shinyApp(
  ui = app_ui(),
  server = function(input, output, session) {
    app_server(input, output, session,
               loans = loans, model = model_bundle$model, scale = model_bundle$scale,
               aml_flags = aml_flags)
  }
)
