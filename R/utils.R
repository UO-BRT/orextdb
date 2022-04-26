#' internal paste function
#' Note: created by DA and not currently used
#' Infix function to paste long strings together a little easier
#'
#' @keywords internal
#' @param lhs = character vector to paste on left
#' @param rhs = character vector to paste on right
#' @return character vector comprised of \code{lhs} & \code{rhs}
`%p%` <- function(lhs, rhs) {
  paste0(lhs, rhs)
}

#' internal tests - determining if tibble is installed
#'
#' @keywords internal
#' @noRd
is_tibble_installed <- function() {
  requireNamespace("tibble", quietly = TRUE)
}

#' explain how to format the database specification when misspecified
#'
#' @keywords internal
#' @param type = character vector indicating either c('message', 'stop')
#' which passes to respective functions
#' @param reason = character vector indicating the way they misspecified
#'  (changes Note 1 of 2)
#' @return message about database formatting

explain_db_format <-
  function(type, reason){
    general_message <-
        paste("\nNOTE 2: `db` argument must specify a 4-digit year, with the first two " ,
        "digits representing the start of the school year, and the " ,
        "last two digits representing the end of the school year. `db` may ",
        "be passed with or without the `\"ORExt\"` prefix, e.g., `\"1920\"` " ,
        "or `\"ORExt1920\"`.", sep = '\n')

    if (reason == 'incorrect_num_digits') {
      specific_message <- 'NOTE 1: User specified incorrect number of digits '
    }

    if (reason == 'digits_only') {
      specific_message <- 'NOTE 1: User only provided digits '
    }
    if (reason == 'incorrect_format') {
      specific_message <- 'NOTE 1: User format is incorrect '
    }
    if (reason == 'invalid_years') {
      specific_message <- 'NOTE 1: User specified invalid combination of years '
    }



    written_message <-
      paste(
        specific_message,
        general_message,
        sep = '\n'
      )

    if (type == 'stop'){
      stop(
        written_message,
        call. = FALSE
      )
      }

    if (type == 'message'){
      message(
        written_message
      )
    }
    }
