library(shiny)
library(shinyalert)

fluidPage(
  useShinyalert(),
  uiOutput("lobby_scene"),
  uiOutput("game_scene")
)
