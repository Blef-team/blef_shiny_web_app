generate_name <- function() {
  paste0(sample(names$adjective, 1), "_", sample(names$animal, 1))
}

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

format_all_hands <- function(hands) {
  if (length(hands) == 0) {
    return(NULL)
  } else {
    return(
      do.call(
        rbind,
        lapply(hands, function(p) 
          data.frame(
            Player = p$nickname,
            Cards = format_hand(p$hand)
          )
        )
      )
    )
  }
}

format_hand <- function(hand) {
  lapply(hand, function(card) {
    as.character(img(src = paste0("assets/cards/specific/", card$value, card$colour, ".png"), height = 40))
  }) %>%
    paste0(collapse = "")
}

format_players <- function(players) {
  players %>%
    unlist() %>%
    matrix(nrow = length(players), byrow = T) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    set_colnames(c("Player", "Cards"))
}

format_history <- function(history) {
  if (length(history) == 0) {
    data.frame(message = "There hasn't been any bet yet.")
  } else {
    history %>%
      unlist() %>%
      matrix(nrow = length(history), byrow = T) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      set_colnames(c("Player", "Action ID")) %>%
      mutate(`Action ID` = sapply(`Action ID`, function(id) actions$description[as.numeric(id) + 1]))
  }
}

check_if_move_needed <- function(nickname, game) {
  output <- FALSE
  # Check if the user is a player
  if (!is.null(nickname)) { 
    # Check if their nickname matches the current nickname
    if (catch_null(nickname) == catch_null(game$cp_nickname)) {
      # Check if the history is either empty ...
      if (length(game$history) == 0) {
        output <- TRUE
        # ... or in other ways indicates that the round hasn't ended
      } else if (as.numeric(last(game$history)$action_id) != 89) {
        output <- TRUE
      }
    }
  }
  return(output)
}

put_variables_in_URL <- function(game_uuid, player_uuid, nickname) {
  updateQueryString(paste0("?game_uuid=", game_uuid, "&player_uuid=", player_uuid, "&nickname=", nickname))
}

# When putting variables in the URL, NULL variables are converted to empty parameters. We have to convert them back to NULL.
map_empty_strings_to_null <- function(list) {
  return(
    lapply(list, function(x) {
      if (x == "") {
        return(NULL)
      } else {
        return(x)
      }
    })
  )
}
