library(shiny)
library(shinyalert)
library(shiny.router)

fluidPage(
  includeCSS("www/style.css"),
  useShinyalert(),
  router_ui()
)
