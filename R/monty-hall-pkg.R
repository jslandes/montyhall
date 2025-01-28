#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'   Contestant selects a door in Monty Hall Program game.
#' @description
#'   `select_door()` randomly selects a door to mimic a contestant making 
#'   their first selection in a Monty Hall Problem game.
#' @details
#'   The function randomly selects one of the three doors (labeled 1, 2, or 3) 
#'   that represents the contestant's choice. The selection is made by 
#'   generating a random number between 1 and 3. This is the first step in the 
#'   Monty Hall Problem game.
#' @param ... no arguments are used by the function.
#' @return An integer (1, 2, or 3) representing the door selected by the 
#'   contestant.
#' @examples
#'   select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Host opens a goat door in Monty Hall Problem game.
#' @description
#'   `open_goat_door()` simulates the action of a game host opening a door
#'   with a goat after the contestant makes a pick in the Monty Hall problem.
#' @details
#'   This function simulates the action of the host in the Monty Hall Problem 
#'   game. The host opens one of the remaining goat doors after the contestant 
#'   makes their initial pick. Based on the contestant's door selection from 
#'   `select_door()`, the remaining doors the host can open will either have 
#'   two goats or one goat and one car. This function considers the 
#'   contestant's initial choice and returns a door number that the host opens 
#'   (which will always have a goat).
#' @param `game` is a length 3 character vector of 3 game items that show two 
#'   goats and one car in any order. These represent what's behind each of the 
#'   doors. `a.pick` is an integer (1, 2, or 3) representing the contestant's 
#'   initial door selection from `select_door()`.
#' @return An integer (1, 2, or 3) representing the door number that the host 
#'   opens, which will always contain a goat.
#' @examples
#'   open_goat_door()
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'   Contestant chooses final door in Monty Hall Problem game.
#' @description
#'   `change_door()` simulates what happens when a contestant is given the 
#'   choice to keep their initial selection or switch to the remaining closed 
#'   door after the host eliminates a door with a goat behind it.
#' @details
#'   In the Monty Hall Problem game, the contestant chooses 1 of 3 doors, then 
#'   the host opens one of the remaining doors to reveal a goat. This leaves 
#'   2 closed doors--the contestant's initial selection and a door they did 
#'   not initially select. The contestant decides whether to keep their 
#'   initial selection or to switch doors. This function simulates that will 
#'   happen for both choices.
#' @param `stay` is a logical value indicating whether a contestant wants to 
#'   stay with their initial selection (`TRUE`) or switch to the  remaining 
#'   door (`FALSE`). `opened.door` is an integer (1, 2, or 3) representing the 
#'   door the host opened to reveal a goat in `open_goat_door`. `a.pick` is an 
#'   integer (1, 2, or 3) representing the contestant's initial door selection 
#'   from `select_door()`.
#' @return An integer (1, 2, or 3) representing the door number that the 
#'   contestant ends up with after deciding to keep or switch.
#' @examples
#'   change_door()
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#'   Determine the winner in a Monty Hall Problem game.
#' @description
#'   `determine_winner()` determines if the contestant wins or loses 
#'   based on their final selection in the Monty Hall Problem game.
#' @details
#'   The function compares the contestant's final selection and game setup to 
#'   the win/loss criteria for the game. If the contestant's final selection 
#'   is a car, they win. If the contestant's final selection is a goat, they 
#'   lose.
#' @param `final.pick` is an integer (1, 2, or 3) representing the door number 
#'   that the contestant ends up with after deciding to keep or switch. `game` 
#'   is a length 3 character vector of 3 game items that show two goats and 
#'   one car in any order.
#' @return A character string ("WIN" or "LOSE") determining whether the 
#'   contestant's final selection resulted in a win or loss.
#' @examples
#'   determine_winner()
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'   Monty Hall Problem game simulator.
#' @description
#'   `play_game` simulates a single Monty Hall Problem game from start to 
#'   finish.
#' @details
#'   This function simulates a single Monty Hall Problem game from start to 
#'   finish. Using other package functions, it sets up the game, selects the 
#'   contestant's initial door, reveals a host-selected door with a goat 
#'   behind it, simulates what happens when the contestant keeps their door and 
#'   switches doors, determines the game outcome in both scenarios, and 
#'   returns the outcome.
#' @param ... no arguments are used by the function.
#' @return A data frame with the outcomes for both the "stay" and "switch" 
#'   strategies. The data frame has two columns where `strategy` represents the 
#'   possible choices from `change_door()` ("stay" or "switch") and `outcome` 
#'   represents the outcomes for each strategy ("win" or "lose").
#' @examples
#'   play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'   Looped Monty Hall Problem game simulator.
#' @description
#'   `play_n_games` simulates multiple Monty Hall Problem games from start to 
#'   finish.
#' @details
#'   This function simulates multiple Monty Hall Problem games from start to 
#'   finish. It creates a data frame of results for a user-defined number of 
#'   games and summarizes the results in a proportion table to show the 
#'   likelihood of winning and losing based on strategy.
#' @param `n` is a numeric value specifying the number of games to play. It 
#'   defaults to 100.
#' @return A data frame with the results of each individual game. The data 
#'   frame has two columns where `strategy` represents the possible choices 
#'   from `change_door()` ("stay" or "switch") and `outcome` represents the 
#'   outcomes for each strategy ("win" or "lose"). It also returns a proportion 
#'   table summarizing the frequency of wins and losses by strategy.
#' @examples
#'   play_n_games()
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
