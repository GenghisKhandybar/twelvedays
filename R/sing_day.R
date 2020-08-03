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

  phrases <- dataset %>% pull({{phrase_col}})

  day_word <- dataset$Day.in.Words[line]


  all_gifts <- phrases[1:line] %>%
    map(str_c, sep = "\n", collapse=NULL) %>%
    unlist() %>%
    str_c(collapse=NULL)

  start <- glue("On the {day_word} day of Christmas, my true love sent to me,")
  ans1 <- paste(all_gifts[line:1],collapse = "\n")
  ans <- paste(c(start, ans1), collapse = "\n")
  return(ans)

}
