# Internal object storing the names of the database tables, which
# can be referenced in functions
tbls <- c(
  "Accomodations", "Answers", "Districts", "Exams", "Items",
  "Preferences", "Schools", "Students", "Students_old",
  "Submissions", "SupplementalDistricts", "SupplementalSchools",
  "Tasks", "User", "UserStudents", "UserStudents_old"
)

#' Checks the name of the table
#'
#' Returns an error and lists the tables that are acceptable, if a non-expected
#' table name is supplied.
#'
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

#' Paste function
#'
#' Infix function to paste long strings together a little easier
#'
#' @keywords internal
#' @noRd
`%p%` <- function(lhs, rhs) {
  paste0(lhs, rhs)
}

#' Checks the database argument and transforms it if needed
#' @inheritParams db_get
#' @export
check_db <- function(db) {
  if (is.null(db)) {
    return()
  }
  if (grepl("^\\d", db)) {
    if (nchar(db) != 4) {
    stop(
      "`db` argument must specify a 4-digit year, with the first two " %p%
      "digits representing the start of the school year, and the" %p%
      "last two digits representing the end of the school year. `db` may" %p%
      "be passed with or without the `\"ORExt\"` prefix, e.g., `\"1920\"` " %p%
      "or `\"ORExt1920\"`.",
    call. = FALSE
    )
    }
    db <- paste0("ORExt", db)
  }
  if (!grepl("^ORExt\\d\\d\\d\\d$", db)) {
    stop(
      "`db` argument must specify a 4-digit year, with the first two " %p%
      "digits representing the start of the school year, and the" %p%
      "last two digits representing the end of the school year. `db` may" %p%
      "be passed with or without the `\"ORExt\"` prefix, e.g., `\"1920\"` " %p%
      "or `\"ORExt1920\"`.",
    call. = FALSE
    )
  }
  year <- gsub("ORExt", "", db)
  y1 <- as.numeric(substr(year, 1, 2))
  y2 <- as.numeric(substr(year, 3, 4))

  if (y2 - y1 != 1) {
    stop(
      "`db` argument must specify a 4-digit year, with the first two " %p%
      "digits representing the start of the school year, and the" %p%
      "last two digits representing the end of the school year. `db` may" %p%
      "be passed with or without the `\"ORExt\"` prefix, e.g., `\"1920\"` " %p%
      "or `\"ORExt1920\"`.",
    call. = FALSE
    )
  }
  db
}

#' @keywords internal
#' @noRd
is_tibble_installed <- function() {
  requireNamespace("tibble", quietly = TRUE)
}
