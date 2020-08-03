#' Takes a noun and makes it plural
#'
#' @param dataset A data frame containing information about gifts
#' @param line The number of the line for the day you want to sing about
#' @param phrase_col The variable name for the column in the dataset that
#' contains the gift phrases
#'
#' @return A string singing the line of the song with all gifts for the given day.
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export
sing_day <- function(dataset, line, phrase_col){

  # Create first line of complete song including the day
  complete_song <- c()
  complete_song <- c(complete_song,
                     paste("On the", dataset[line, "Day.in.Words"],
                           "day of Christmas, my true love sent to me,"))

  # Check if there's multiple days to determine adding an "and"
  if (line > 1) {
    multiple_flag = 1
  }
  else {
    multiple_flag = 0
  }

  # Create all other lines of the song
  while(line >= 1) {
    # If there are multiple lines and this is the last, add "and"
    if (line == 1 && multiple_flag == 1) {
      complete_song <- c(complete_song, paste("and", (dataset[line, phrase_col])))
    }
    # Else, add complete phrase
    else {
      complete_song <- c(complete_song, dataset[line, phrase_col])
    }
    line = line - 1
  }

  # Return completed song
  return(complete_song)
}
