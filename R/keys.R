#' Access your key from .Renviron
#'
#' This function is primarily used as the default argument for [db_get()], but
#' can be run at any time to access your specific key. Note that you should
#' first set your key with [db_set_key()].
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' library(orextdb)
#' db_key()
#' }
#'
#' @return Either your api key or a message explaining how to load it

db_key <- function() {
  key <- Sys.getenv("or_ext_api_token")
  if(nchar(key) < 1) {
    return(stop(paste0(
      "Key not found in R Environment. Please inspect ",
      "`usethis::edit_r_environ()` and ensure your api token is called ",
      "'or_ext_api_token' or provide the key to the function directly."
    ), call. = FALSE
    ))
  }
  key
}

#' Function to set a key for accessing the live OR Extended database
#'
#' This function adds your key to your .Renviron so it is accessible by
#' [db_key()]. You should only need to run this function once. Make sure to
#' restart your local R session after running this function for the changes
#' to take effect.
#'
#' @param key A string of your specific key
#' @param overwrite Defaults to \code{FALSE}. If there is a key already in the
#'   .Renviron, should it be overwritten?
#' @export
#' @examples
#' library(orextdb)
#'
#' \dontrun{
#' db_set_key("abcdefghikjlmnop")
#' }
#' @return A message or warning
db_set_key <- function(key, overwrite = FALSE) {
  token <- paste0('or_ext_api_token = "', key, '"')

  # create .Renviron if it doesn't already exist
  home <- path.expand("~")
  home_files <- list.files(home, all.files = TRUE)

  if (!any(grepl("\\.Renviron", home_files))) {
    file.create(file.path(home, ".Renviron"))
  }

  renviron <- readLines(file.path(home, ".Renviron"))
  current_token <- grep("or_ext_api_token", renviron)

  if (length(current_token) > 0) {
    if (overwrite) {
      renviron[current_token] <- token
    } else {
      stop("`or_ext_api_token` already exists in .Renviron. ",
           "Run again with `overwrite = TRUE` to overwrite the existing key.",
           call. = FALSE)
    }
  } else {
    placement <- length(renviron) + 1
    renviron[placement] <- token
  }
  writeLines(renviron, file.path(home, ".Renviron"))
  message("Make sure to restart R for changes to take effect")
}
