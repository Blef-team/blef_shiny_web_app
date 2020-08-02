library(shiny)
library(shinyalert)
library(shiny.router)

fluidPage(
  uiOutput("style"),
  includeCSS("www/style.css"),
  useShinyalert(),
  router_ui()
)
