library(shiny)
library(shinyalert)
library(magrittr)
library(readr)
library(stringr)
library(httr)
library(dplyr)
library(digest)

base_path <- "http://18.132.35.89:8001/v2/"
source("nicknames.R")
actions <- read_csv("action_descriptions.csv", col_types = cols())
source("routines.R", local = TRUE)

shinyServer(function(input, output, session) {
  
  scene <- reactiveVal("lobby")
  action_initialised <- reactiveVal("none")
  game_uuid <- reactiveVal(NULL)
  player_uuid <- reactiveVal(NULL)
  nickname <- reactiveVal(NULL)
  game <- reactiveValues()
  games <- reactiveVal()
  new_round_available <- reactiveVal(FALSE)
  data_recovered <- reactiveVal(FALSE)
  game_md5 <- reactiveVal("")
  games_md5 <- reactiveVal("")
  
  try_enter_game_room <- function(game_uuid_wanted, player_uuid_wanted, nickname_wanted) {
    response <- try(GET(paste0(base_path, "games/", game_uuid_wanted, "?player_uuid=", player_uuid_wanted)), silent = TRUE)
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    } else if (status_code(response) != 200) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)$error))
    } else {
      game_uuid(game_uuid_wanted)
      player_uuid(player_uuid_wanted)
      nickname(nickname_wanted)
      put_variables_in_URL(game_uuid(), player_uuid(), nickname())
      lapply(names(content(response)), function(x) game[[x]] <- content(response)[[x]])
      new_round_available(FALSE)
      scene("game")
      action_initialised("none")
    }
  }
  
  try_update_to_current_state <- function(game_uuid, player_uuid, round = -1) {
    response <- try(GET(paste0(base_path, "games/", game_uuid, "?player_uuid=", player_uuid, "&round=", round)), silent = TRUE)
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    } else if (status_code(response) != 200) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)$error))
    } else {
      lapply(names(content(response)), function(x) game[[x]] <- content(response)[[x]])
    }
  }
  
  observe({
    if (!data_recovered()) {
      if (session$clientData$url_search != "") {
        query <- parseQueryString(session$clientData$url_search) %>%
          map_empty_strings_to_null()
        try_enter_game_room(query$game_uuid, query$player_uuid, query$nickname)
        data_recovered(TRUE)
      }
    }
  })
  
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
          textInput("nickname", HTML("Pick a nickname<br/>(or leave blank and we'll generate one):"), width = 300),
          actionButton("cancel", "Cancel"),
          actionButton("confirm_create", "Confirm")
        )
      } else if (action_initialised() == "join") {
        console <- list(
          textInput("game_uuid", "Game's UUID:", width = 300, placeholder = "00000000-0000-0000-0000-000000000000"),
          textInput("nickname", HTML("Pick a nickname<br/>(or leave blank and we'll generate one):"), width = 300),
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
      if (digest(content(response)) != games_md5()) {
        games_md5(digest(content(response)))
        if (is_empty_response(response)) {
          shinyalert("Error", "There was an error querying the game engine")
        } else if (length(content(response)) > 0) {
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
        } else {
          games(NULL)
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
          try_enter_game_room(created_game_uuid, content(response)$player_uuid, effective_nickname)
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
      lower_case_uuid <- str_to_lower(input$game_uuid)
      effective_nickname <- if (input$nickname == "") generate_name() else input$nickname
      response <- try(GET(paste0(base_path, "games/", lower_case_uuid, "/join?nickname=", effective_nickname)), silent = TRUE)
      if (is_empty_response(response)) {
        shinyalert("Error", "There was an error querying the game engine")
      } else if (status_code(response) != 200) {
        shinyalert("Error", paste0("The engine returned an error saying: ", content(response)$error))
      } else {
        try_enter_game_room(lower_case_uuid, content(response)$player_uuid, effective_nickname)
      }
    }
  })
  
  observeEvent(input$return, {
    scene("game")
  })
  
  observeEvent(input$observe, {
    action_initialised("observe")
  })
  
  observeEvent(input$confirm_observe, {
    lower_case_uuid <- str_to_lower(input$game_uuid)
    try_enter_game_room(lower_case_uuid, NULL, NULL)
  })
  
  observeEvent(input$cancel, {
    action_initialised("none")
  })
  
  output$game_scene <- renderUI({
    if (scene() == "game") {
      
      leave_button <- if (game$status == "Finished" | is.null(player_uuid())) list(
        actionButton("leave", "Leave to lobby"),
        br(),
        br()
      )
      
      if (game$status == "Not started") {
        game_info_table <- data.frame(
          keys = c("Game UUID", "Admin nickname", "Game public"),
          values = as.character(c(game_uuid(), game$admin_nickname, ifelse(game$public, "Yes", "No")))
        )
        
        players_table <- format_players(game$players)$Player
        
        admin_panel <- if (catch_null(nickname()) == game$admin_nickname) {
          list(
            start_button <- actionButton("start", "Start game"),
            privacy_button <- if (!game$public) actionButton("make_public", "Make public"),
            privacy_button <- if (game$public) actionButton("make_private", "Make private")
          )
        }
        
        return(
          mainPanel(
            br(),
            leave_button,
            renderTable(game_info_table, include.colnames = FALSE),
            h5("Players:"),
            renderTable(players_table, include.colnames = FALSE),
            h5("The game has not started yet."),
            admin_panel
          )
        )
      } else {
        
        general_info <- data.frame(
          keys = c("Game public", "Maximum allowed cards"),
          values = as.character(c(ifelse(game$public, "Yes", "No"), game$max_cards))
        )
        
        history_table <- list(
          h5("History:"),
          renderTable(format_history(game$history), include.colnames = FALSE)
        )
        
        cp_info <- if (!is.null(game$cp_nickname) & catch_null(game$cp_nickname) != catch_null(nickname())) {
          h5(paste0("Current player: ", game$cp_nickname))
        }
        
        if (game$status == "Running") {
          if (length(game$hands) == 1) {
            # If you only see your own hand, generate both a 'your hand' row and a 'cards per player' object
            game_info <- list(
              renderTable(general_info, include.colnames = FALSE, sanitize.text.function = function(x) x), 
              h5("Cards per player:"),
              renderTable(format_players(game$players), include.colnames = FALSE, sanitize.text.function = function(x) x),
              history_table,
              h5("Your hand:"), 
              renderUI(HTML(format_hand(game$hands[[1]]$hand)))
            )
          } else if (length(game$hands) > 1) {
            # If you can show everybody's (more than 1 person's) hands, don't show cards per player
            game_info <- list(
              renderTable(general_info, include.colnames = FALSE, sanitize.text.function = function(x) x), 
              h5("Cards per player:"),
              renderTable(format_players(game$players), include.colnames = FALSE, sanitize.text.function = function(x) x),
              history_table,
              h5("Hands:"),
              renderTable(format_all_hands(game$hands), include.colnames = FALSE, sanitize.text.function = function(x) x)
            )
          } else if (length(game$hands) == 0) {
            # If you can't show anybody's hand, there is no hand object to generate
            game_info <- list(
              renderTable(general_info, include.colnames = FALSE, sanitize.text.function = function(x) x), 
              h5("Cards per player:"),
              renderTable(format_players(game$players), include.colnames = FALSE, sanitize.text.function = function(x) x),
              history_table
            )  
          }
        } else if (game$status == "Finished") {
          game_info <- list(
            renderTable(general_info, include.colnames = FALSE, sanitize.text.function = function(x) x), 
            h5("Results:"),
            renderTable(format_players(game$players), include.colnames = FALSE, sanitize.text.function = function(x) x),
            h5("Hands:"),
            renderTable(format_all_hands(game$hands), include.colnames = FALSE, sanitize.text.function = function(x) x),
            history_table
          )
        }
        
        action_menu <- if (!is.null(nickname()) & catch_null(nickname()) == catch_null(game$cp_nickname)) {
          last_bet <- if (length(game$history) > 0) last(game$history)$action_id else -1
          
          list(
            h5("Make your move:"),
            if (last_bet < 87) selectInput("bet_id", NULL, setNames((last_bet + 1):87, actions$description[(last_bet + 2):88])),
            if (last_bet < 87) actionButton("bet", "Confirm bet"),
            if (length(game$history) > 0) actionButton("check", "Check")
          )
        }
        
        update_button <- if(new_round_available()) {
          list(
            br(),
            actionButton("update_game", "Go to latest round")
          )
        }
        
        status_message <- if (game$status == "Finished") {
          h5("The game has finished.")
        } else if (length(game$hands) == 1 | length(game$hands) == 0) {
          cp_info
        }
        
        return(
          mainPanel(
            br(),
            leave_button,
            game_info,
            action_menu,
            update_button,
            status_message
          )
        )
      }
    }
  })
  
  # Automatically update the state of the round every 500 miliseconds, but don't automatically display new round
  observe({
    invalidateLater(1000)
    if (scene() == "game" & catch_null(game$status) != "Finished" & !catch_null(new_round_available())) {
      response <- try(GET(paste0(base_path, "games/", game_uuid(), "?player_uuid=", player_uuid())), silent = TRUE)
      if (digest(content(response)) != game_md5()) {
        game_md5(digest(content(response)))
        if (is_empty_response(response)) {
          shinyalert("Error", "There was an error querying the game engine")
        } else if (status_code(response) != 200) {
          shinyalert("Error", paste0("The engine returned an error saying: ", content(response)$error))
        } else {
          # If a game hasn't progressed to another round, just update the info
          if (catch_null(game$status) == "Not started" | (content(response)$status == "Running" & content(response)$round_number == catch_null(game$round_number))) {
            lapply(names(content(response)), function(x) game[[x]] <- content(response)[[x]])
          } else {
            # If a game has progressed to another round, update info for the last round seen by user and inform user that new round is available
            response <- try(GET(paste0(base_path, "games/", game_uuid(), "?player_uuid=", player_uuid(), "&round=", game$round_number)), silent = TRUE)
            if (is_empty_response(response)) {
              shinyalert("Error", "There was an error querying the game engine")
            } else if (status_code(response) != 200) {
              shinyalert("Error", paste0("The engine returned an error saying: ", content(response)$error))
            } else {
              lapply(names(content(response)), function(x) game[[x]] <- content(response)[[x]])
            }
            new_round_available(TRUE)
          }
        }
      }
    }
  })
  
  # Manually update the game to the latest state
  observeEvent(input$update_game, {
    response <- try(GET(paste0(base_path, "games/", game_uuid(), "?player_uuid=", player_uuid())), silent = TRUE)
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    } else if (status_code(response) != 200) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)$error))
    } else {
      lapply(names(content(response)), function(x) game[[x]] <- content(response)[[x]])
      new_round_available(FALSE)
    }
  })
  
  observeEvent(input$leave, {
    scene("lobby")
  })
  
  observeEvent(input$start, {
    response <- try(GET(paste0(base_path, "games/", game_uuid(), "/start?admin_uuid=", player_uuid())), silent = TRUE)
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    } else if (status_code(response) != 202) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)$error))
    }
    try_update_to_current_state(game_uuid(), player_uuid())
  })
  
  observeEvent(input$make_public, {
    response <- try(GET(paste0(base_path, "games/", game_uuid(), "/make-public?admin_uuid=", player_uuid())), silent = TRUE)
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    } else if (status_code(response) != 200) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)$error))
    }
    try_update_to_current_state(game_uuid(), player_uuid())
  })
  
  observeEvent(input$make_private, {
    response <- try(GET(paste0(base_path, "games/", game_uuid(), "/make-private?admin_uuid=", player_uuid())), silent = TRUE)
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    } else if (status_code(response) != 200) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)$error))
    }
    try_update_to_current_state(game_uuid(), player_uuid())
  })
  
  observeEvent(input$bet, {
    response <- try(GET(paste0(base_path, "games/", game_uuid(), "/play?player_uuid=", player_uuid(), "&action_id=", input$bet_id)), silent = TRUE)
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    } else if (status_code(response) != 200) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)$error))
    }
    try_update_to_current_state(game_uuid(), player_uuid(), game$round_number)
  })
  
  observeEvent(input$check, {
    response <- try(GET(paste0(base_path, "games/", game_uuid(), "/play?player_uuid=", player_uuid(), "&action_id=", 88)), silent = TRUE)
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    } else if (status_code(response) != 200) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)$error))
    }
    try_update_to_current_state(game_uuid(), player_uuid(), game$round_number)
  })
})
