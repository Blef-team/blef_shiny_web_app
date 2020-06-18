generate_name <- function() {
  letter <- sample(1:26, 1)
  paste0(sample(nicknames[[letter]]$adjectives, 1), "_", sample(nicknames[[letter]]$animal, 1))
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
  player_table <- players %>%
    unlist() %>%
    matrix(nrow = length(players), byrow = T) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    set_colnames(c("Player", "Cards"))
  
  # Check if some players have already lost
  if (any(player_table$Cards == "0")) {
    # Check if the game is still going on
    if (sum(player_table$Cards != "0") > 1) {
      player_table$Cards <- sapply(player_table$Cards, function(n) {
        text <- if (n == "0") "Lost" else n
        return(text)
      })
    } else {
      # If the game has finished
      player_table$Cards <- sapply(player_table$Cards, function(n) {
        text <- if (n == "0") "<b>Lost</b>" else "<b>Won</b>"
        return(text)
      })
    }
  }
  return(player_table)
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
