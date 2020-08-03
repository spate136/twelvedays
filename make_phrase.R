#' Takes a noun and makes it plural
#'
#' @param num An integer
#' @param num_word A string corresponding to the integer
#' @param item A string
#' @param verb A string
#' @param adjective A string
#' @param location A string
#'
#' @return A string containing the words in grammatical order.
#'
#' @import stringr
#' @import glue
#' @import dplyr
#' @import purrr
#'
#' @export
make_phrase <- function(num, num_word, item, verb, adjective, location){

  phrases <- c()

  # Replace NA with empty strings
  verb[is.na(verb)] <- ""
  adjective[is.na(adjective)] <- ""
  location[is.na(location)] <- ""

  # Iterate through strings, format based on types given
  for (i in 1:length(num)) {
    # Verb given
    if (verb[i] != "") {
      phrases[i] <- paste(words(num[i]), pluralize_gift(item[i]), verb[i], sep = " ", collapse = NULL)
    }
    # Adjective given
    else if (adjective[i] != "") {
      phrases[i] <- paste(words(num[i]), adjective[i], pluralize_gift(item[i]), sep = " ", collapse = NULL)
    }
    # Location given (i.e. special case for first day)
    else if (location[i] != "" && i == 1) {
      # Basic vowel check to determine usage of "a" or "an".
      check_vowel <- substr(item[i], 1 , 1)
      if (check_vowel == "a" | check_vowel == "e" | check_vowel == "i" | check_vowel == "o" | check_vowel == "o") {
        phrases[i] <- paste("an", item[i], location[i], sep = " ", collapse = NULL)
      }
      else {
        phrases[i] <- paste("a", item[i], location[i], sep = " ", collapse = NULL)
      }
    }
  }

  # Return completed phrases
  return(phrases)
}
