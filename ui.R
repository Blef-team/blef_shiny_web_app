library(shiny)
library(shinyalert)

fluidPage(
  includeCSS("www/style.css"),
  useShinyalert(),
  uiOutput("lobby_scene"),
  uiOutput("game_scene")
)
