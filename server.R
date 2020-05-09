library(shiny)
library(shinyalert)
library(magrittr)
library(readr)
library(stringr)
library(httr)
library(dplyr)

base_path <- "http://18.132.35.89:8001/v2/"

names <- read_csv("names.csv", col_types = cols())
generate_name <- function() {
  paste0(sample(names$adjective, 1), "_", sample(names$animal, 1))
}

actions <- read_csv("action_descriptions.csv", col_types = cols())

is_empty_response <- function(response) {
  if (length(response) == 1 & is.character(response) & str_detect(response[1], "Error in curl")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
} 

catch_null = function(x) {
  ifelse(length(x) == 0, "Not available", x)
}

shinyServer(function(input, output) {
  scene <- reactiveVal("lobby")
  action_initialised <- reactiveVal("none")
  game_uuid <- reactiveVal(NULL)
  player_uuid <- reactiveVal(NULL)
  nickname <- reactiveVal(NULL)
  game <- reactiveVal()
  games <- reactiveVal()
  
  game_info_loaded <- reactiveVal(FALSE)
  
  output$lobby_scene <- renderUI({
    if (scene() == "lobby") {
      if (action_initialised() == "none") {
        console <- list(
          actionButton("create", "Create"),
          actionButton("join", "Join"),
          if (!is.null(game_uuid())) actionButton("return", "Return to game"),
          actionButton("observe", "Observe")
        )
      } else if (action_initialised() == "create") {
        console <- list(
          textInput("nickname", HTML("Pick a nickname<br/>(or leave blank and let us generate one):"), width = 300),
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
        h4("Public games:"),
        renderTable({
          if (length(games()) > 0) games()
        }, sanitize.text.function = function(x) str_remove(x, "/"))
      )
    }
  })
  
  observe({
    invalidateLater(1000)
    if (scene() == "lobby") {
      response <- try(GET(paste0(base_path, "games")), silent = TRUE)
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
              set_colnames(c("UUID", "Players", "Started")) %>%
              mutate(UUID = sapply(UUID, function(x) HTML(paste0("<div style=\"font-family: Consolas\">", x, "</div>"))))
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
    } else {
      response <- try(GET(paste0(base_path, "games/create")), silent = TRUE)
      if (is_empty_response(response)) {
        shinyalert("Error", "There was an error querying the game engine")
      } else if (status_code(response) != 200) {
        shinyalert("Error", paste0("The engine returned an error saying: ", content(response)))
      } else {
        created_game_uuid <- content(response)$game_uuid
        effective_nickname <- if (input$nickname == "") generate_name() else input$nickname
        response <- try(GET(paste0(base_path, "games/", created_game_uuid, "/join?nickname=", effective_nickname)), silent = TRUE)
        if (is_empty_response(response)) {
          shinyalert("Error", paste0("The game was created with UUID ", created_game_uuid, " but, while trying to join, there was an error querying the game engine"))
        } else if (status_code(response) != 200) {
          shinyalert("Error", paste0("The game was created with UUID ", created_game_uuid, " but, while trying to join, the engine returned an error saying: ", content(response)$error))
        } else {
          game_uuid(created_game_uuid)
          player_uuid(content(response)$player_uuid)
          nickname(effective_nickname)
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
    if (input$nickname != "" & !str_detect(input$nickname, "^[a-zA-Z]\\w*$")) {
      shinyalert("Invalid nickname", "It won't be possible to join with this nickname. A nickname must start with a letter and only have alphanumeric characters")
    } else {
      effective_nickname <- if (input$nickname == "") generate_name() else input$nickname
      response <- try(GET(paste0(base_path, "games/", input$game_uuid, "/join?nickname=", effective_nickname)), silent = TRUE)
      if (is_empty_response(response)) {
        shinyalert("Error", "There was an error querying the game engine")
      } else if (status_code(response) != 200) {
        shinyalert("Error", paste0("The engine returned an error saying: ", content(response)$error))
      } else {
        game_uuid(input$game_uuid)
        player_uuid(content(response)$player_uuid)
        nickname(effective_nickname)
        scene("game")
        action_initialised("none")
      }
    }
  })
  
  observeEvent(input$return, {
    scene("game")
    action_initialised("none")
  })
  
  observeEvent(input$observe, {
    action_initialised("observe")
  })
  
  observeEvent(input$confirm_observe, {
    response <- try(GET(paste0(base_path, "games/", input$game_uuid)), silent = TRUE)
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    } else if (status_code(response) != 200) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)$error))
    } else {
      game_uuid(input$game_uuid)
      player_uuid(NULL)
      nickname(NULL)
      scene("game")
      action_initialised("none")
    }
  })
  
  observeEvent(input$cancel, {
    action_initialised("none")
  })
  
  output$game_scene <- renderUI({
    if (game_info_loaded()) {
      game_info_table <- data.frame(
        keys = c("Game UUID", "Admin nickname", "Game public", "Game status", "Current round number", "Maximum allowed cards", "Current player"),
        values = as.character(c(game_uuid(), game()$admin_nickname, ifelse(game()$public, "Yes", "No"), game()$status, game()$round_number, game()$max_cards, catch_null(game()$cp_nickname)))
      )
      
      pre_game_buttons <- if (game()$status == "Not started" & catch_null(nickname()) == game()$admin_nickname) {
        list(
          br(),
          start_button <- actionButton("start", "Start game"),
          privacy_button <- if (!game()$public) actionButton("make_public", "Make public"),
          privacy_button <- if (game()$public) actionButton("make_private", "Make private")
        )
      }
      
      mainPanel(
        br(),
        if (game()$status == "Finished" | is.null(player_uuid())) list(
          actionButton("leave", "Leave to lobby"),
          br(),
          br()
        ),
        renderTable(game_info_table, include.colnames = FALSE),
        pre_game_buttons,
        if (length(game()$players) > 0) {
          list(
            h5("Cards per player:"),
            renderTable({
              game()$players %>%
                unlist() %>%
                matrix(nrow = length(game()$players), byrow = T) %>%
                data.frame(stringsAsFactors = FALSE) %>%
                set_colnames(c("Player", "Cards"))
            }, include.colnames = FALSE)
          )
        },
        if (length(game()$hands) > 0) {
          list(
            h5("Known cards:"),
            renderTable({
              do.call(
                rbind,
                lapply(game()$hands, function(p) 
                  do.call(rbind, p$hand) %>%
                    as.data.frame() %>%
                    set_colnames(c("Value", "Colour")) %>%
                    mutate(Player = p$nickname) %>%
                    select(Player, Value, Colour)
                )
              )
            }, include.colnames = FALSE)
          )
        },
        if (length(game()$history) > 0) {
          history <- game()$history
          list(
            h5("History:"),
            renderTable({
              history %>%
                unlist() %>%
                matrix(nrow = length(history), byrow = T) %>%
                data.frame(stringsAsFactors = FALSE) %>%
                set_colnames(c("Player", "Action ID"))
            }, include.colnames = FALSE)
          )
        },
        if (!is.null(nickname()) & catch_null(nickname()) == catch_null(game()$cp_nickname)) {
          list(
            h5("Make your move:"),
            selectInput("bet_id", NULL, setNames(0:87, head(actions$description, -1)), selected = "Check"),
            actionButton("bet", "Confirm bet"),
            actionButton("check", "Check")
          )
        }
      )
    }
  })
  
  observe({
    invalidateLater(500)
    if (scene() == "game") {
      if (!is.null(player_uuid)) response <- try(GET(paste0(base_path, "games/", game_uuid(), "?player_uuid=", player_uuid())), silent = TRUE)
      if (is.null(player_uuid)) response <- try(GET(paste0(base_path, "games/", game_uuid())), silent = TRUE)
      if (is_empty_response(response)) {
        shinyalert("Error", "There was an error querying the game engine")
      } else if (status_code(response) != 200) {
        shinyalert("Error", paste0("The engine returned an error saying: ", content(response)$error))
      } else {
        game(content(response))
        game_info_loaded(TRUE)
      }
    }
  })
  
  observeEvent(input$leave, {
    scene("lobby")
    game_info_loaded(FALSE)
  })
  
  observeEvent(input$start, {
    response <- try(GET(paste0(base_path, "games/", game_uuid(), "/start?admin_uuid=", player_uuid())), silent = TRUE)
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    } else if (status_code(response) != 202) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)$error))
    }
  })
  
  observeEvent(input$make_public, {
    response <- try(GET(paste0(base_path, "games/", game_uuid(), "/make-public?admin_uuid=", player_uuid())), silent = TRUE)
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    } else if (status_code(response) != 200) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)$error))
    }
  })
  
  observeEvent(input$make_private, {
    response <- try(GET(paste0(base_path, "games/", game_uuid(), "/make-private?admin_uuid=", player_uuid())), silent = TRUE)
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    } else if (status_code(response) != 200) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)$error))
    }
  })
  
  observeEvent(input$bet, {
    response <- try(GET(paste0(base_path, "games/", game_uuid(), "/play?player_uuid=", player_uuid(), "&action_id=", input$bet_id)), silent = TRUE)
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    } else if (status_code(response) != 200) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)$error))
    }
  })
  
  observeEvent(input$check, {
    response <- try(GET(paste0(base_path, "games/", game_uuid(), "/play?player_uuid=", player_uuid(), "&action_id=", 88)), silent = TRUE)
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    } else if (status_code(response) != 200) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)$error))
    }
  })
})
