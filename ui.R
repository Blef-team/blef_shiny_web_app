library(shiny)
library(shinyalert)
library(shiny.router)

redirect_js <- "Shiny.addCustomMessageHandler('redirecter', function(message) { window.location = message;});"

fluidPage(
  tags$head(tags$script(redirect_js)),
  includeCSS("www/darkly.css"),
  uiOutput("style"),
  includeCSS("www/style.css"),
  useShinyalert(),
  router_ui()
)
