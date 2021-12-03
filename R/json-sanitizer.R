#' Swaps out a pattern between the first/last characters of a string
#' @param pat A pattern to search for within the inner part of the string
#' @param replace What the pattern should be replaced with
#' @param string The string to search/replace
#' @param replace_end Optional, defaults to \code{FALSE}. Should the end of the
#'   string be replaced as well?
#' @keywords internal
#' @noRd
gsub_within <- function(pat, replace, string, replace_end = FALSE) {
  if (nchar(string) <= 1 | nchar(string) < 1) {
    return(string)
  }

  start_loc <- 1
  end_loc <- nchar(string)

  beg <- substr(string, start_loc, start_loc)
  if (beg == "{") {
    start_loc <- 2
    beg <- substr(string, 1, start_loc)
  }

  last <- substr(string, end_loc, end_loc)
  if (last == "}") {
    end_loc <- end_loc - 1
    last <- substr(string, end_loc, nchar(string))
  }
  if (replace_end & end_loc == nchar(string)) {
      last <- gsub(pat, replace, last)
  }

  inner <- substr(string, start_loc + 1, end_loc - 1)
  inner <- gsub(pat, replace, inner)
  paste0(beg, inner, last)
}

#' Cleans up the JSON so it can be read into R. Basically handles quotes a 
#' little better.
#' @param json The JSON to be read in
#' @return The JSON with all `\"` instances within sentences replaced with `'`
#' @keywords internal
#' @noRd
sanitize_json <- function(json) {
  # split
  tmp <- strsplit(json, '",')[[1]]
  tmp <- lapply(tmp, function(x) strsplit(x, '\":', fixed = TRUE)[[1]])

  # fix
  keys <- lapply(tmp, "[", 1)
  keys <- lapply(keys, function(x) ifelse(is.na(x), "", x))

  data <- lapply(tmp, "[", 2)
  data <- lapply(data, function(x) ifelse(is.na(x), "", x))

  keys <- lapply(keys, function(x) {
    vapply(
      x,
      function(y) gsub_within('\\\"', "'", y, replace_end = TRUE),
      FUN.VALUE = character(1)
    )
  })

  data <- lapply(data, function(x) {
    vapply(
      x,
      function(y) gsub_within('\\\"', "'", y, replace_end = TRUE),
      FUN.VALUE = character(1)
    )
  })

  # put it back together
  out <- Map(function(keys, data) {
    paste0(keys, '\":', data)
  },
  keys = keys,
  data = data)

  # remove any obs with missing key and data
  out[vapply(out, function(x) x == "\":", FUN.VALUE = logical(1))] <- ""

  paste0(unlist(out), collapse = '",')
}

#' Converts a single JSON string
#' @keywords internal
#' @noRd
convert_json_ <- function(json) {
  json <- gsub("\\s\\\"\\s", " ", json)
  tryCatch(
    fromJSON(json),
    error = function(e) {
      fromJSON(sanitize_json(json))
    }
  )
}

#' converts a list of JSON
#' @param json_l A list of JSON
#' @return A list of data frames from the JSON data.
#' @keywords internal
#' @noRd
convert_json <- function(json_l) {
  lapply(json_l, convert_json_)
}
