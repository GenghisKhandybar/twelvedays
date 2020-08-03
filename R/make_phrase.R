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



make_phrase <- function(num, item, verb, adjective, location){
  #Instead of taking num_word as a parameter, it is automatically
  #generated here. This is suited to the task as the word for one
  #can only be replaced with "and a" this way.
  nums <- c("and a", "two", "three", "four", "five", "six", "seven",
            "eight", "nine", "ten", "eleven", "twelve")
  num_word <- nums[num]


  verb <- str_replace_na(verb, "")
  adjective <- str_replace_na(adjective, "")
  location <- str_replace_na(location, "")

  return(
    str_c(num_word, adjective, item, verb, location, sep=" ") %>%
      str_replace_all("  ", " ") %>%
      str_replace_all(" $", "")
  )

}

