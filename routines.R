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