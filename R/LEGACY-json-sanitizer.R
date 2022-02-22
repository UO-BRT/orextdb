# Note this entire file is legacy code that is probably not needed anymore
# I am keeping in case we ever need to pull anything from the JSON data, but
# it can probably be deleted pretty safely.

#' Cleans up the JSON so it can be read into R. Basically handles quotes a 
#' little better.
#' @param json The full JSON to be read in
#' @return JSON with only the item difficulty values
#' @keywords internal
#' @noRd
get_json_diffs <- function(json) {
  # split
  tmp <- strsplit(json, '",')[[1]]
  tmp <- lapply(tmp, function(x) strsplit(x, '\":', fixed = TRUE)[[1]])

  # fix
  keys <- lapply(tmp, "[", 1)
  keys <- lapply(keys, function(x) ifelse(is.na(x), "", x))

  # find the location of the difficulties  
  diff <- vapply(
    keys, {
      function(x) grepl("[Dd]iff\\d\\d", x)
    },
     FUN.VALUE = logical(1)
  )

  # subset to only these, remove excess characters
  keys <- lapply(keys[diff], sanitize)

  # do the same thing with the data
  data <- lapply(tmp, "[", 2)
  data <- lapply(data, function(x) ifelse(is.na(x), "", x))
  data <- lapply(data[diff], sanitize)

  # put it back together
  out <- Map(function(keys, data) {
    paste0(wrap_quotes(keys), " : ", wrap_quotes(data))
  },
  keys = keys,
  data = data)

  paste0("{", paste0(unlist(out), collapse = ","), "}")
}

sanitize <- function(x) {
  trimws(gsub("\\{|\\}|\\\"", "", x), "both")
}

wrap_quotes <- function(x) {
  paste0("\"", x, "\"")
}

#' converts a list of JSON
#' @param json_l A list of JSON
#' @return A list of data frames from the JSON data.
#' @keywords internal
#' @noRd
convert_json <- function(json_l) {
  lapply(json_l, function(x) fromJSON(get_json_diffs(x)))
}


#' Pulls only the first estimated item difficulty for every item difficulty
#' listed in the data frame
#'
#' Note this is legacy code that is probably not needed anymore
#'
#' @param l The full list of converted JSON data - i.,e., the output from
#'   [convert_json()]
#' @keywords internal
#' @noRd
get_difficulties <- function(l) {
  diff_pattern <- "diff\\d\\d$"

  vapply(l, function(x) {
    difficulties <- x[grepl(diff_pattern, names(x))]
    suppressWarnings(
      difficulties <- lapply(difficulties, as.numeric)
    )
    difficulties <- difficulties[order(names(difficulties))]

    idx <- seq_along(difficulties)
    not_missing <- !is.na(difficulties)

    first <- idx[not_missing][1]
    if (is.na(first)) {
      return(NA_real_)
    }

    difficulties[[first]]
  },
  FUN.VALUE = double(1)
  )
}