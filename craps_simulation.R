# craps_simulation.R

# Helper Functions
roll_dice <- function() {
  total <- sum(sample(1:6, 2, replace = TRUE))
  return(total)
}

is_come_out_roll <- function(roll){
  return(roll %in% c(2,3,7,11,12))
}

handle_come_out_roll <- function(roll){
  if (roll %in% c(7,11)) {
    return("You win")
  } else if (roll %in% c(2,3,12)){
    return("You lose")
  } else {
    return("point")
  }
}

handle_point_roll <- function(point){
  while(TRUE){
    roll <- roll_dice()
    if (roll == point){
      return("You win")
    } else if (roll == 7){
      return("You lose")
    }
  }
}

# Main Functions
simulate_craps_game <- function() {
  game_data <- data.frame(
    Roll_Number = integer(),
    Die_1 = integer(),
    Die_2 = integer(),
    Total = integer(),
    Roll_Type = character(),
    Point = character(),
    Outcome = character(),
    Game_Status = character(),
    Play_Again = character(),
    stringsAsFactors = FALSE
  )

  point <- NA
  roll_number <- 1
  play_again <- TRUE

  while (play_again) {
    die_1 <- sample(1:6, 1)
    die_2 <- sample(1:6, 1)
    total <- die_1 + die_2

    if (is.na(point)) { # Come-Out Roll
      roll_type <- "Come-Out Roll"
      if (total %in% c(7, 11)) {
        outcome <- "You Win"
        game_status <- "End"
        play_again <- FALSE
        point <- "N/A"
      } else if (total %in% c(2, 3, 12)) {
        outcome <- "You Lose"
        game_status <- "End"
        play_again <- FALSE
        point <- "N/A"
      } else {
        outcome <- "Point Set"
        game_status <- "Continue"
        point <- total
      }
    } else { # Point Roll
      roll_type <- "Point Roll"
      if (total == point) {
        outcome <- "You Win"
        game_status <- "End"
        play_again <- FALSE
      } else if (total == 7) {
        outcome <- "You Lose"
        game_status <- "End"
        play_again <- FALSE
      } else {
        outcome <- "Rolling to hit point"
        game_status <- "Continue"
      }
    }

    game_data <- rbind(game_data, data.frame(
      Roll_Number = roll_number,
      Die_1 = die_1,
      Die_2 = die_2,
      Total = total,
      Roll_Type = roll_type,
      Point = point,
      Outcome = outcome,
      Game_Status = game_status,
      Play_Again = ifelse(play_again, "Yes", "No"),
      stringsAsFactors = FALSE
    ))

    roll_number <- roll_number + 1
  }

  return(game_data)
}

summarize_craps_game <- function(game_data) {
  n_rolls <- nrow(game_data)
  outcome <- game_data$Outcome[n_rolls]
  point <- if (any(game_data$Roll_Type == "Point Roll")) {
    game_data$Point[game_data$Roll_Type == "Point Roll"][1]
  } else {
    NA
  }

  return(data.frame(
    n_rolls = n_rolls,
    outcome = outcome,
    point = point,
    stringsAsFactors = FALSE
  ))
}


run_craps_simulation <- function(N) {
  all_game_summaries <- data.frame(
    game_id = integer(),
    n_rolls = integer(),
    outcome = character(),
    point = integer(),
    stringsAsFactors = FALSE
  )

  for (i in 1:N) {
    game_data <- simulate_craps_game()  # Simulate one game
    game_summary <- summarize_craps_game(game_data)  # Summarize the game
    game_summary$game_id <- i  # Add the game ID
    all_game_summaries <- rbind(all_game_summaries, game_summary)  # Combine summaries
  }

  return(all_game_summaries[, c("game_id", "n_rolls", "outcome", "point")])
}












