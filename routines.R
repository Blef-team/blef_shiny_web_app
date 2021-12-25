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

format_nickname <- function(nickname, own_nickname) {
  ifelse(nickname == own_nickname, paste0("<b>", own_nickname, "</b>"), nickname) %>%
    str_replace_all("_", " ")
}

format_players_not_started <- function(players, own_nickname = "") {
  player_table <- do.call(rbind, lapply(players, unlist)) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    select(Player = nickname) %>%
    mutate(Player = sapply(Player, function(x) format_nickname(x, own_nickname)))
  return(player_table)
}

format_players_running <- function(players, hands, max_cards, own_nickname = "") {
  player_table <- do.call(rbind, lapply(players, unlist)) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    select( nickname, n_cards)
  player_table <- data.frame(info = sapply(1:nrow(player_table), function(i) {
    nickname <- player_table$nickname[i]
    n_cards <- player_table$n_cards[i]
    formatted_nickname <- format_nickname(nickname, own_nickname)
    if (n_cards == 0) formatted_nickname <- paste0("<s>", formatted_nickname, "</s>")
    hand_index <- sapply(hands, function(hand) hand$nickname == nickname)
    if (any(hand_index)) {
      hand <- hands[hand_index][[1]]$hand
      formatted_hand <- format_open_hand(hand, as.numeric(max_cards))
    } else {
      formatted_hand <- format_closed_hand(as.numeric(n_cards), as.numeric(max_cards))
    }
    return(paste0(formatted_nickname, "<br>", formatted_hand))
  }))
  return(player_table)
}

format_players_finished <- function(players, own_nickname = "") {
  player_table <- do.call(rbind, lapply(players, unlist)) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    select(Player = nickname, Cards = n_cards) %>%
    mutate(Player = sapply(Player, function(x) format_nickname(x, own_nickname))) %>%
    mutate(Cards = sapply(Cards, function(n) ifelse(n == "0", "<b>Lost</b>", "<b>Won</b>")))
  return(player_table)
}

format_closed_hand <- function(n_cards, max_cards) {
  question_cards <- rep(
    "<img src=\"assets/cardQuestion.png\" height=\"45\">",
    n_cards
  )
  empty_cards <- rep(
    "<img src=\"assets/cardEmpty2.png\" height=\"45\" style=\"opacity: 35%;\">",
    max_cards - n_cards
  )
  return(paste0(c(question_cards, empty_cards), collapse = ""))
}

format_open_hand <- function(hand, max_cards) {
  open_cards <- lapply(hand, function(card) {
    as.character(img(src = paste0("assets/cards/cropped/", card$value, card$colour, ".png"), height = 45))
  }) %>%
    paste0(collapse = "")
  empty_cards <- rep(
    "<img src=\"assets/cardEmpty2.png\" height=\"45\" style=\"opacity: 35%;\">",
    max_cards - length(hand)
  )
  return(paste0(c(open_cards, empty_cards), collapse = ""))
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
    # Highlight own nickname
    if (!own_nickname == "") {
      formatted$Player <- sapply(formatted$Player, function(x) format_nickname(x, own_nickname))
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
