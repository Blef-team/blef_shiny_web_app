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

format_all_hands <- function(hands, own_nickname = "") {
  if (length(hands) == 0) {
    return(NULL)
  } else {
    list_of_hands <- lapply(
      hands, 
      function(p) {
        effective_nickname <- if_else(p$nickname == own_nickname, "You", p$nickname)
        data.frame(Player = effective_nickname, Cards = format_hand(p$hand))
      }
    )
    return(do.call(rbind, list_of_hands))
  }
}

format_hand <- function(hand) {
  lapply(hand, function(card) {
    as.character(img(src = paste0("assets/cards/cropped/", card$value, card$colour, ".png"), height = 45))
  }) %>%
    paste0(collapse = "")
}

format_players <- function(players, own_nickname = "") {
  player_table <- do.call(rbind, lapply(players, unlist)) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    select(Player = nickname, Cards = n_cards)

  # Mark own nickname as 'You'
  if (!own_nickname == "") {
    player_table$Player[player_table$Player == own_nickname] <- "You"
  }
  
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

format_history <- function(history, own_nickname = "") {
  if (length(history) == 0) {
    return(data.frame(message = "There hasn't been any bet yet."))
  } else {
    formatted <- do.call(rbind, lapply(history, unlist)) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      select(Player = player, `Action ID` = action_id) %>%
      filter(`Action ID` != 89) %>%
      mutate(`Action ID` = sapply(`Action ID`, function(id) actions$description[as.numeric(id) + 1]))
    # Mark own nickname as 'You'
    if (!own_nickname == "") {
      formatted$Player[formatted$Player == own_nickname] <- "You"
    }
    return(formatted)
  }
}

make_URL_for_lobby <- function(game_uuid, player_uuid, nickname, dark_mode) {
  paste0("?game_uuid=", game_uuid, "&player_uuid=", player_uuid, "&nickname=", nickname, "&dark_mode=", dark_mode, "#!/lobby")
}

make_URL_for_join <- function(game_uuid, dark_mode) {
  paste0("?game_uuid=", game_uuid, "&dark_mode=", dark_mode, "#!/join")
}

make_URL_for_game <- function(game_uuid, player_uuid, nickname, dark_mode) {
  paste0("?game_uuid=", game_uuid, "&player_uuid=", player_uuid, "&nickname=", nickname, "&dark_mode=", dark_mode, "#!/play")
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
