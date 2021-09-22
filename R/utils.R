tbls <- c("Accomodations", "Answers", "Districts", "Exams", "Items",
          "Preferences", "Schools", "Students", "Students_old",
          "Submissions", "SupplementalDistricts", "SupplementalSchools",
          "Tasks", "User", "UserStudents", "UserStudents_old")

#' @keywords internal
#' @noRd
check_tables <- function(tbl) {
  if (!tbl %in% tbls) {
    stop("The table you requested is not part of the database. Please request ",
         "one of the following tables:\n",
         paste0(tbls, "\n"),
         call. = FALSE)
  }
}

#' @keywords internal
#' @noRd
is_tibble_installed <- function() {
  requireNamespace("tibble", quietly = TRUE)
}
