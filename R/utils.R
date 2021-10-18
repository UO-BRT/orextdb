tbls <- c("Accomodations", "Answers", "Districts", "Exams", "Items",
          "Preferences", "Schools", "Students", "Students_old",
          "Submissions", "SupplementalDistricts", "SupplementalSchools",
          "Tasks", "User", "UserStudents", "UserStudents_old")

#' @keywords internal
#' @noRd
check_tables <- function(tbl) {
  if (!tbl %in% tbls) {
    stop("The table you requested is not part of the database.\nPlease request ",
         "one of the following tables:\n",
         paste0("* ", tbls, "\n"),
         call. = FALSE)
  }
}

#' @keywords internal
#' @noRd
is_tibble_installed <- function() {
  requireNamespace("tibble", quietly = TRUE)
}

#' Pulls only the first estimated item difficulty for every item difficulty
#' listed in the data frame
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