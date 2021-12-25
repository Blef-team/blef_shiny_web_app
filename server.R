library(shiny)
library(shinyalert)
library(magrittr)
library(readr)
library(stringr)
library(httr)
library(dplyr)
library(digest)
library(shiny.router)
library(DT)

base_path <- "https://n4p6oovxsg.execute-api.eu-west-2.amazonaws.com/"
source("nicknames.R")
actions <- read_csv("action_descriptions.csv", col_types = cols())
source("routines.R", local = TRUE)

lobby_scene <- div(
  titlePanel("Blef - game lobby"),
  uiOutput("lobby_control_panel"),
  uiOutput("style_checkbox"),
  hr(),
  h4("Public games:"),
  uiOutput("games_table")
)

join_scene <- div(
  uiOutput("style_checkbox"),
  uiOutput("join_ui")
)

game_scene <- div(
  uiOutput("leave_button"),
  uiOutput("style_checkbox"),
  uiOutput("game_general_info"),
  uiOutput("players_table"),
  uiOutput("history_table"),
  uiOutput("hands"),
  uiOutput("bet_menu"),
  uiOutput("game_status_message"),
  uiOutput("round_status_message"),
  uiOutput("update_button"),
  uiOutput("admin_panel")
)

redirect_scene <- div()

lobby_server <- function(input, output, session) {
  action_initialised <- reactiveVal("none")
  games <- reactiveVal()
  games_md5 <- reactiveVal("")
  
  dark_mode <- reactive({
    if (is.null(get_query_param()$dark_mode)) {
      return(FALSE)
    } else {
      as.logical(get_query_param()$dark_mode)
    }
  })
  
  output$style_checkbox <- renderUI({
    checkboxInput("dark_mode", "Dark theme", value = dark_mode())
  })
  
  output$style <- renderUI({
    if (!is.null(input$dark_mode)) {
      if (input$dark_mode) {
        includeCSS("www/darkly.css")
      } else {
        includeCSS("www/flatly.css")
      }
    }
  })
  
  try_enter_game_room <- function(game_uuid_wanted, player_uuid_wanted, nickname_wanted) {
    response <- try(GET(paste0(base_path, "games/", game_uuid_wanted, "?player_uuid=", player_uuid_wanted)), silent = TRUE)
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    } else if (status_code(response) != 200) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)))
    } else {
      url <- make_URL_for_game(game_uuid_wanted, player_uuid_wanted, nickname_wanted, input$dark_mode)
      session$sendCustomMessage("redirecter", url)
    }
  }
  
  output$lobby_control_panel <- renderUI({
    if (action_initialised() == "none") {
      output <- list(
        actionButton("create", "Create"),
        actionButton("join", "Join"),
        actionButton("observe", "Observe")
      )
    } else if (action_initialised() == "create") {
      output <- list(
        textInput("nickname", HTML("Pick a nickname<br/>(or leave blank and we'll generate one):"), width = 300),
        actionButton("cancel", "Cancel"),
        actionButton("confirm_create", "Confirm")
      )
    } else if (action_initialised() == "join") {
      output <- list(
        textInput("game_uuid", "Game's UUID:", width = 300, placeholder = "00000000-0000-0000-0000-000000000000"),
        textInput("nickname", HTML("Pick a nickname<br/>(or leave blank and we'll generate one):"), width = 300),
        actionButton("cancel", "Cancel"),
        actionButton("confirm_join", "Confirm")
      )
    } else if (action_initialised() == "observe") {
      output <- list(
        textInput("game_uuid", "Game's UUID", width = 300, placeholder = "00000000-0000-0000-0000-000000000000"),
        actionButton("cancel", "Cancel"),
        actionButton("confirm_observe", "Confirm")
      )
    }
    
    return(output)
  })
  
  output$games_table <- renderUI({
    if(!is.null(games())) {
      list(
        renderDT(
          games() %>% select(-UUID),
          style = "bootstrap",
          rownames = FALSE,
          colnames = c("Room", "Players", "", ""),
          escape = FALSE,
          options = list(
            ordering = FALSE,
            dom = "t"
          )
        )
      )
    }
  }) 
  
  create_join_button <- function(id) {
    as.character(actionButton(paste0("button_", id), label = "Join", onclick = 'Shiny.onInputChange(\"join_from_table\", this.id)'))
  } 
  create_observe_button <- function(id) {
    as.character(actionButton(paste0("button_", id), label = "Observe", onclick = 'Shiny.onInputChange(\"observe_from_table\", this.id)'))
  }
  
  observe({
    invalidateLater(1000)
    response <- try(GET(paste0(base_path, "games")), silent = TRUE)
    if (digest(content(response)) != games_md5()) {
      games_md5(digest(content(response)))
      if (is_empty_response(response)) {
        shinyalert("Error", "There was an error querying the game engine")
      } else if (length(content(response)) > 0) {
        raw_games <- content(response)
        for (i in 1:length(raw_games)) raw_games[[i]]$players <- paste(raw_games[[i]]$players, collapse = ", ")
        renamed_cols <- lapply(raw_games, function(game) {
          data.frame(
            UUID = game$game_uuid,
            Room = game$room,
            Players = paste(game$players, collapse = ", ")
          )
        })
        games(
          do.call(rbind, renamed_cols) %>%
            mutate(
              Join = sapply(1:nrow(.), function(i) create_join_button(i)),
              Observe = sapply(1:nrow(.), function(i) create_observe_button(i))
            )
        )
      } else {
        games(NULL)
      }
    }
  })
  
  observeEvent(input$join_from_table, {
    row <- as.numeric(strsplit(input$join_from_table, "_")[[1]][2])
    game_uuid <- games()$UUID[row]
    url <- make_URL_for_join(game_uuid, input$dark_mode)
    session$sendCustomMessage("redirecter", url)
  })
  
  observeEvent(input$observe_from_table, {
    row <- as.numeric(strsplit(input$observe_from_table, "_")[[1]][2])
    try_enter_game_room(games()$UUID[row], NULL, NULL)
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
          shinyalert("Error", paste0("The game was created with UUID ", created_game_uuid, " but, while trying to join, the engine returned an error saying: ", content(response)))
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
        shinyalert("Error", paste0("The engine returned an error saying: ", content(response)))
      } else {
        try_enter_game_room(lower_case_uuid, content(response)$player_uuid, effective_nickname)
      }
    }
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
}

join_server <- function(input, output, session) {
  game_uuid <- reactive(get_query_param()$game_uuid)
  
  dark_mode <- reactive({
    if (is.null(get_query_param()$dark_mode)) {
      return(FALSE)
    } else {
      as.logical(get_query_param()$dark_mode)
    }
  })
  
  output$style_checkbox <- renderUI({
    checkboxInput("dark_mode", "Dark theme", value = dark_mode())
  })
  
  output$style <- renderUI({
    if (!is.null(input$dark_mode)) {
      if (input$dark_mode) {
        includeCSS("www/darkly.css")
      } else {
        includeCSS("www/flatly.css")
      }
    }
  })
  
  output$join_ui <- renderUI({
    list(
      h5(HTML(paste0("You are joining game <b>", game_uuid(), "<b/>."))),
      hr(),
      textInput("nickname", HTML("Pick a nickname<br/>(or leave blank and we'll generate one):"), width = 300),
      actionButton("leave", "Cancel"),
      actionButton("confirm", "Confirm")
    )
  })
  
  observeEvent(input$leave, {
    url <- make_URL_for_lobby("", "", "", input$dark_mode)
    session$sendCustomMessage("redirecter", url)
  })
  
  try_enter_game_room <- function(game_uuid_wanted, player_uuid_wanted, nickname_wanted) {
    response <- try(GET(paste0(base_path, "games/", game_uuid_wanted, "?player_uuid=", player_uuid_wanted)), silent = TRUE)
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    } else if (status_code(response) != 200) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)))
    } else {
      url <- make_URL_for_game(game_uuid_wanted, player_uuid_wanted, nickname_wanted, input$dark_mode)
      session$sendCustomMessage("redirecter", url)
    }
  }
  
  observeEvent(input$confirm, {
    if (input$nickname != "" & !str_detect(input$nickname, "^[a-zA-Z]\\w*$")) {
      shinyalert("Invalid nickname", "It won't be possible to join with this nickname. A nickname must start with a letter and only have alphanumeric characters")
    } else {
      lower_case_uuid <- str_to_lower(game_uuid())
      effective_nickname <- if (input$nickname == "") generate_name() else input$nickname
      response <- try(GET(paste0(base_path, "games/", lower_case_uuid, "/join?nickname=", effective_nickname)), silent = TRUE)
      if (is_empty_response(response)) {
        shinyalert("Error", "There was an error querying the game engine")
      } else if (status_code(response) != 200) {
        shinyalert("Error", paste0("The engine returned an error saying: ", content(response)))
      } else {
        try_enter_game_room(lower_case_uuid, content(response)$player_uuid, effective_nickname)
      }
    }
  })
}

game_server <- function(input, output, session) {
  game <- reactiveValues()
  new_round_available <- reactiveVal(FALSE)
  game_md5 <- reactiveVal("")
  
  game_uuid <- reactive({
    if (is.null(get_query_param()$game_uuid)) {
      return(NULL)
    } else {
      get_query_param()$game_uuid
    }
  })
  
  player_uuid <- reactive({
    if (is.null(get_query_param()$player_uuid)) {
      return(NULL)
    } else {
      get_query_param()$player_uuid
    }
  })
  
  nickname <- reactive({
    if (is.null(get_query_param()$nickname)) {
      return(NULL)
    } else {
      get_query_param()$nickname
    }
  })
  
  dark_mode <- reactive({
    if (is.null(get_query_param()$dark_mode)) {
      return(FALSE)
    } else {
      as.logical(get_query_param()$dark_mode)
    }
  })
  
  output$style_checkbox <- renderUI({
    checkboxInput("dark_mode", "Dark theme", value = dark_mode())
  })
  
  output$style <- renderUI({
    if (!is.null(input$dark_mode)) {
      if (input$dark_mode) {
        includeCSS("www/darkly.css")
      } else {
        includeCSS("www/flatly.css")
      }
    }
  })
  
  response <- try(GET(paste0(base_path, "games/", game_uuid(), "?player_uuid=", player_uuid())), silent = TRUE)
  lapply(names(content(response)), function(x) game[[x]] <- content(response)[[x]])
  
  try_update_to_current_state <- function(game_uuid, player_uuid, round = -1) {
    if(round < 1) {
      response <- try(GET(paste0(base_path, "games/", game_uuid, "?player_uuid=", player_uuid)), silent = TRUE)
    } else {
      response <- try(GET(paste0(base_path, "games/", game_uuid, "?player_uuid=", player_uuid, "&round=", round)), silent = TRUE)
    }
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    } else if (status_code(response) != 200) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)))
    } else {
      lapply(names(content(response)), function(x) game[[x]] <- content(response)[[x]])
    }
  }
  
  output$leave_button <- renderUI({
    if (game$status == "Finished" | player_uuid() == "")
      list(
        br(),
        actionButton("leave", "Leave to lobby")
      )
  })
  
  output$game_general_info <- renderUI({
    if (game$status == "Not started") {
      info_table <- data.frame(
        keys = c("Room", "Admin nickname"),
        values = as.character(c(ifelse(game$public == "true", game$room, "Private"), format_nickname(game$admin_nickname, catch_null(nickname()))))
      )
      list(renderTable(info_table, include.colnames = FALSE, sanitize.text.function = function(x) x))
    }
  })
  
  output$players_table <- renderUI({
    if (game$status == "Not started") {
      list(
        h5("Players:"),
        renderTable(
          format_players_not_started(game$players, nickname()), 
          include.colnames = FALSE, 
          sanitize.text.function = function(x) x
        )
      )
    } else if (game$status == "Running") {
      list(
        h5("Cards per player:"),
        renderTable(format_players(game$players, nickname()), include.colnames = FALSE, sanitize.text.function = function(x) x)
      )
    } else if (game$status == "Finished") {
      list(
        h5("Results:"),
        renderTable(format_players(game$players, nickname()), include.colnames = FALSE, sanitize.text.function = function(x) x)
      )
    }
  })
  
  output$history_table <- renderUI({
    if (game$status == "Running") {
      list(
        h5("Actions so far:"),
        renderTable(format_history(game$history, nickname()), include.colnames = FALSE, sanitize.text.function = function(x) x)
      )
    }
  })
  
  output$hands <- renderUI({
    if (game$status == "Running") {
      if (length(game$hands) == 1) {
        list(
          h5("Your hand:"), 
          renderUI(HTML(format_hand(game$hands[[1]]$hand)))
        )
      } else if (length(game$hands) > 1) {
        list(
          h5("Hands:"),
          renderTable(
            format_all_hands(game$hands, nickname()), 
            include.colnames = FALSE, 
            sanitize.text.function = function(x) x
          )
        )
      }
    }
  })
  
  output$bet_menu <- renderUI({
    if (game$status == "Running" & nickname() != "" & nickname() == catch_null(game$cp_nickname)) {
      last_bet <- if (length(game$history) > 0) last(game$history)$action_id else -1
      
      list(
        h5("Make your move:"),
        if (last_bet < 87) selectInput("bet_id", NULL, setNames((last_bet + 1):87, actions$description[(last_bet + 2):88])),
        if (last_bet < 87) actionButton("bet", "Confirm bet"),
        if (length(game$history) > 0) actionButton("check", "Check")
      )
    }
  })
  
  output$game_status_message <- renderUI({
    if (game$status == "Not started") {
      h5("The game has not started yet.")
    } else if (game$status == "Finished") {
      h5("The game has finished.")
    }
  })
  
  output$round_status_message <- renderUI({
    if (
      game$status == "Running" &
      !is.null(game$cp_nickname) & 
      catch_null(game$cp_nickname) != catch_null(nickname()) &
      length(game$hands) <= 1
    ) {
      h5(paste0("Current player: ", game$cp_nickname))
    } else if (length(game$history) > 2) {
      if (last(game$history)$action_id == 89) {
        effective_nickname <- format_nickname(last(game$history)$player, nickname())
        h5(HTML(paste0(effective_nickname, " lost the round.")))
      }
    }
  })
  
  output$update_button <- renderUI({
    if (game$status == "Running" & new_round_available()) {
      actionButton("update_game", "Go to latest round")
    }
  })
  
  output$admin_panel <- renderUI({
    if (game$status == "Not started" & catch_null(nickname()) == game$admin_nickname) {
      list(
        start_button <- if (length(game$players) >= 2) actionButton("start", "Start game"),
        privacy_button <- if (game$public == "false") actionButton("make_public", "Make public"),
        privacy_button <- if (game$public == "true") actionButton("make_private", "Make private"),
        invite_dazhbog_button <- actionButton("invite_dazhbog", "Invite Dazhbog (AI)")
      )
    }
  })
  
  # Automatically update the state of the round every 500 miliseconds, but don't automatically display new round
  observe({
    invalidateLater(1000)
    if (catch_null(game$status) != "Finished" & !catch_null(new_round_available())) {
      response <- try(GET(paste0(base_path, "games/", game_uuid(), "?player_uuid=", player_uuid())), silent = TRUE)
      if (digest(content(response)) != game_md5()) {
        game_md5(digest(content(response)))
        if (is_empty_response(response)) {
          shinyalert("Error", "There was an error querying the game engine")
        } else if (status_code(response) != 200) {
          shinyalert("Error", paste0("The engine returned an error saying: ", content(response)))
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
              shinyalert("Error", paste0("The engine returned an error saying: ", content(response)))
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
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)))
    } else {
      lapply(names(content(response)), function(x) game[[x]] <- content(response)[[x]])
      new_round_available(FALSE)
    }
  })
  
  observeEvent(input$leave, {
    url <- make_URL_for_lobby("", "", "", input$dark_mode)
    session$sendCustomMessage("redirecter", url)
  })
  
  observeEvent(input$start, {
    response <- try(GET(paste0(base_path, "games/", game_uuid(), "/start?admin_uuid=", player_uuid())), silent = TRUE)
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    } else if (status_code(response) != 202) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)))
    }
    try_update_to_current_state(game_uuid(), player_uuid())
  })
  
  observeEvent(input$make_public, {
    response <- try(GET(paste0(base_path, "games/", game_uuid(), "/make-public?admin_uuid=", player_uuid())), silent = TRUE)
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    } else if (status_code(response) != 200) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)))
    }
    try_update_to_current_state(game_uuid(), player_uuid())
  })
  
  observeEvent(input$make_private, {
    response <- try(GET(paste0(base_path, "games/", game_uuid(), "/make-private?admin_uuid=", player_uuid())), silent = TRUE)
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    } else if (status_code(response) != 200) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)))
    }
    try_update_to_current_state(game_uuid(), player_uuid())
  })
  
  observeEvent(input$invite_dazhbog, {
    response <- try(GET(paste0(base_path, "games/", game_uuid(), "/invite-aiagent?admin_uuid=", player_uuid(), "&agent_name=Dazhbog")), silent = TRUE)
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    } else if (status_code(response) != 200) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)))
    }
    try_update_to_current_state(game_uuid(), player_uuid())
  })
  
  observeEvent(input$bet, {
    response <- try(GET(paste0(base_path, "games/", game_uuid(), "/play?player_uuid=", player_uuid(), "&action_id=", input$bet_id)), silent = TRUE)
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    }
    try_update_to_current_state(game_uuid(), player_uuid(), game$round_number)
  })
  
  observeEvent(input$check, {
    response <- try(GET(paste0(base_path, "games/", game_uuid(), "/play?player_uuid=", player_uuid(), "&action_id=", 88)), silent = TRUE)
    if (is_empty_response(response)) {
      shinyalert("Error", "There was an error querying the game engine")
    } else if (status_code(response) != 200) {
      shinyalert("Error", paste0("The engine returned an error saying: ", content(response)))
    }
    try_update_to_current_state(game_uuid(), player_uuid(), game$round_number)
  })
}

redirect_server <- function(input, output, session) {
  change_page("lobby")
}

router <- make_router(
  route("lobby", lobby_scene, lobby_server),
  route("join", join_scene, join_server),
  route("play", game_scene, game_server),
  default = route("/", redirect_scene, redirect_server)
)

shinyServer(function(input, output, session) {
  router(input, output, session)
})
