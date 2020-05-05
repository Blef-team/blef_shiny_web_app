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
  game_uuid <- reactiveVal(NULL)
  player_uuid <- reactiveVal(NULL)
  nickname <- reactiveVal(NULL)
  game <- reactiveVal()
  games <- reactiveVal()
  
  output$lobby_scene <- renderUI({
    if (scene() == "lobby") {
      if (action_initialised() == "none") {
        console <- list(
          actionButton("create", "Create"),
          actionButton("join", "Join"),
          actionButton("rejoin", "Rejoin"),
          actionButton("observe", "Observe")
        )
      } else if (action_initialised() == "create") {
        console <- list(
          textInput("nickname", HTML("Pick a nickname<br/>(or leave blank and join manually later):"), width = 300),
          actionButton("cancel", "Cancel"),
          actionButton("confirm_create", "Confirm")
        )
      } else if (action_initialised() == "join") {
        console <- list(
          textInput("game_uuid", "Game's UUID:", width = 300, placeholder = "00000000-0000-0000-0000-000000000000"),
          textInput("nickname", "Pick a nickname:", width = 300),
          actionButton("cancel", "Cancel"),
          actionButton("confirm_join", "Confirm")
        )
      } else if (action_initialised() == "rejoin") {
        console <- list(
          textInput("game_uuid", "Game's UUID", width = 300, placeholder = "00000000-0000-0000-0000-000000000000"),
          textInput("player_uuid", "Your player UUID", width = 300, placeholder = "00000000-0000-0000-0000-000000000000"),
          actionButton("cancel", "Cancel"),
          actionButton("confirm_rejoin", "Confirm")
        )
      } else if (action_initialised() == "observe") {
        console <- list(
          textInput("game_uuid", "Game's UUID", width = 300, placeholder = "00000000-0000-0000-0000-000000000000"),
          actionButton("cancel", "Cancel"),
          actionButton("confirm_observe", "Confirm")
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
    if (input$nickname != "" & !str_detect(input$nickname, "^[a-zA-Z]\\w*$")) {
      shinyalert(
        "Invalid nickname", 
        "It won't be possible to join with this nickname. 
        A nickname must start with a letter and only have alphanumeric characters.<br/><br/>
        Alternatively, leave the field blank if you don't wish to join the game but rather create an empty one",
        html = TRUE
      )
    } else if (input$nickname == "") {
      response <- try(GET(handle = engine, path = "v2/games/create"), silent = TRUE)
      if (is_empty_response(response)) {
        shinyalert("Error", "There was an error querying the game engine")
      } else if (status_code(response) != 200) {
        shinyalert("Error", paste0("The engine returned an error saying: ", content(response))) 
      } else {
        shinyalert("Game created", paste0("Note the UUID of the game:<br/>", content(response)$game_uuid), html = TRUE) 
        action_initialised("none")
      }
    } else {
      response <- try(GET(handle = engine, path = "v2/games/create"), silent = TRUE)
      if (is_empty_response(response)) {
        shinyalert("Error", "There was an error querying the game engine")
      } else if (status_code(response) != 200) {
        shinyalert("Error", paste0("The engine returned an error saying: ", content(response)))
      } else {
        created_game_uuid <- content(response)$game_uuid
        response <- try(GET(handle = engine, path = paste0("v2/games/", created_game_uuid, "/join?nickname=", input$nickname)), silent = TRUE)
        if (is_empty_response(response)) {
          shinyalert("Error", paste0("The game was created with UUID ", created_game_uuid, " but, while trying to join, there was an error querying the game engine"))
        } else if (status_code(response) != 200) {
          shinyalert("Error", paste0("The game was created with UUID ", created_game_uuid, " but, while trying to join, the engine returned an error saying: ", content(response)$error))
        } else {
          game_uuid(created_game_uuid)
          player_uuid(content(response)$player_uuid)
          nickname(input$nickname)
          scene("game")
          action_initialised("none")
        }
      }
    }
  })
  
  observeEvent(input$join, {
    action_initialised("join")
  })
  
  observeEvent(input$confirm_join, {
    if (!str_detect(input$nickname, "^[a-zA-Z]\\w*$")) {
      shinyalert("Invalid nickname", "It won't be possible to join with this nickname. A nickname must start with a letter and only have alphanumeric characters")
    } else {
      response <- try(GET(handle = engine, path = paste0("v2/games/", input$game_uuid, "/join?nickname=", input$nickname)), silent = TRUE)
      if (is_empty_response(response)) {
        shinyalert("Error", "There was an error querying the game engine")
      } else if (status_code(response) != 200) {
        shinyalert("Error", paste0("The engine returned an error saying: ", content(response)$error))
      } else {
        game_uuid(input$game_uuid)
        player_uuid(content(response)$player_uuid)
        nickname(input$nickname)
        scene("game")
        action_initialised("none")
      }
    }
  })
  
  observeEvent(input$rejoin, {
    action_initialised("rejoin")
  })
  
  observeEvent(input$confirm_rejoin, {
    response <- try(GET(handle = engine, path = paste0("v2/games/", input$game_uuid, "?player_uuid=", input$player_uuid)), silent = TRUE)
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    } else if (status_code(response) != 200) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)$error))
    } else {
      game_uuid(input$game_uuid)
      player_uuid(input$player_uuid)
      scene("game")
      action_initialised("none")
    }
  })
  
  observeEvent(input$observe, {
    action_initialised("observe")
  })
  
  observeEvent(input$confirm_observe, {
    response <- try(GET(handle = engine, path = paste0("v2/games/", input$game_uuid)), silent = TRUE)
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    } else if (status_code(response) != 200) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)$error))
    } else {
      game_uuid(input$game_uuid)
      scene("game")
      action_initialised("none")
    }
  })
  
  observeEvent(input$cancel, {
    action_initialised("none")
  })
  
  output$game_scene <- renderUI({
    if (scene() == "game") {
      mainPanel(
        titlePanel("Blef - game room"),
        actionButton("leave", "Leave to lobby"),
        hr(),
        renderUI({
          HTML(
            paste0(
              "Game UUID: ", game_uuid(), "<br/>",
              "Admin nickname: ", game()$admin_nickname, "<br/>",
              "Game public: ", game()$public, "<br/>",
              "Game status: ", game()$status, "<br/>",
              "Current round number: ", game()$round_number, "<br/>",
              "Maximum allowed cards: ", game()$max_cards, "<br/>",
              "Current player: ", game()$cp_nickname, "<br/>"
            )
          )
        }),
        HTML("Players:<br/>"),
        renderTable({
          if (length(game()$players) > 0) {
            game()$players %>%
              unlist() %>%
              matrix(nrow = length(game()$players), byrow = T) %>%
              data.frame(stringsAsFactors = FALSE) %>%
              set_colnames(c("Player", "Cards"))
          }
        }),
        HTML("Hands:<br/>"),
        renderTable({
          if (length(game()$hands) > 0) {
            do.call(
              rbind,
              lapply(content(temp)$hands, function(p) 
                do.call(rbind, p$hand) %>%
                  as.data.frame() %>%
                  set_colnames(c("Value", "Colour")) %>%
                  dplyr::mutate(Player = p$nickname) %>%
                  dplyr::select(Player, Value, Colour)
              )
            )
          }
        }),
        HTML("History:<br/>"),
        renderTable({
          if (length(game()$history) > 0) {
            game()$history %>%
              unlist() %>%
              matrix(nrow = length(game()$history), byrow = T) %>%
              data.frame(stringsAsFactors = FALSE) %>%
              set_colnames(c("Player", "Action ID"))
          }
        })
      )
    }
  })
  
  observe({
    invalidateLater(1000)
    if (scene() == "game") {
      response <- try(GET(handle = engine, path = paste0("v2/games/", game_uuid())), silent = TRUE)
      if (is_empty_response(response)) {
        shinyalert("Error", "There was an error querying the game engine")
      } else if (status_code(response) != 200) {
        shinyalert("Error", paste0("The engine returned an error saying: ", content(response)$error))
      } else {
        game(content(response))
      }
    }
  })
  
  observeEvent(input$leave, {
    game_uuid(NULL)
    player_uuid(NULL)
    nickname(NULL)
    scene("lobby")
  })
})
