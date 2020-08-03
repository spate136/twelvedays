#' Takes a noun and makes it plural
#'
#' @param gift A string or vector of strings
#'
#' @return A string or vector of strings with the pluralized words
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export
pluralize_gift <- function(gift){

  for (i in 1:length(gift)) {
    # Plural for type "lady" -> "ladies"
    if (str_detect(gift[i], "y$")) {
      gift[i] <- str_replace(gift[i], "y$", "ies")
    }
    # Plural for type "goose" -> "geese"
    else if (str_detect(gift[i], "oo")) {
      gift[i] <- str_replace(gift[i], "oo", "ee")
    }
    # Plural for type "ring" -> "rings"
    else {
      gift[i] <- paste(gift[i], "s", sep = "")
    }
  }

  return(gift)
}
