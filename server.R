library(shiny)
library(shinyalert)
library(magrittr)
library(stringr)
library(httr)

engine <- handle("http://18.132.35.89:8001/")

is_empty_response <- function(response) {
  if (length(response) == 1 & is.character(response) & str_detect(response[1], "Error in curl")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
} 

shinyServer(function(input, output) {
  scene <- reactiveVal("lobby")
  action_initialised <- reactiveVal("none")
  games <- reactiveVal()
  
  output$lobby_scene <- renderUI({
    if (scene() == "lobby") {
      if (action_initialised() == "none") {
        console <- list(
          actionButton("create", "Create")
        )
      } else if (action_initialised() == "create") {
        console <- list(
          actionButton("cancel", "Cancel"),
          actionButton("confirm_create", "Confirm")
        )
      } 
      
      mainPanel(
        titlePanel("Blef - game lobby"),
        console,
        hr(),
        HTML("Public games:<br/>"),
        renderTable({
          if (length(games()) > 0) games()
        })
      )
    }
  })
  
  observe({
    invalidateLater(1000)
    if (scene() == "lobby") {
      response <- try(GET(handle = engine, path = "v2/games"), silent = TRUE)
      if (is_empty_response(response)) {
        shinyalert("Error", "There was an error querying the game engine")
      } else {
        if (length(response) > 0) {
          raw_games <- content(response)
          for (i in 1:length(raw_games)) raw_games[[i]]$players <- paste(raw_games[[i]]$players, collapse = ", ")
          games(
            raw_games %>%
              unlist() %>%
              matrix(nrow = length(raw_games), byrow = T) %>%
              data.frame(stringsAsFactors = FALSE) %>%
              set_colnames(c("UUID", "Players", "Started"))
          )
        }
      }
    }
  })
  
  observeEvent(input$create, {
    action_initialised("create")
  })
  
  observeEvent(input$confirm_create, {
    response <- try(GET(handle = engine, path = "v2/games/create"), silent = TRUE)
    if (status_code(response) != 200) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response))) 
    } else {
      shinyalert("Game created", paste0("Note the UUID of the game:<br/>", content(response)$game_uuid), html = TRUE) 
      action_initialised("none")
    }
  })
  
  observeEvent(input$cancel, {
    action_initialised("none")
  })
  
  output$game_scene <- renderUI({
  })

})
