#' Function to query the live OR Extended database and return specific tables
#'
#' @param table A string specifying the specific table from the Oregon Extended
#'   live database. Should be one of "Accomodations", "Answers", "Districts",
#'   "Exams", "Items", "Preferences", "Schools", "Students", "Students_old",
#'   "Submissions", "SupplementalDistricts", "SupplementalSchools", "Tasks",
#'   "User", "UserStudents", or "UserStudents_old".
#' @param db A string specifying the database to query. Defaults to
#'  \code{NULL}, in which case the most recent database is queried. These
#'   names should be specified either as a four digit year (e.g., 1718) or as
#'   in the database, e.g., \code{"ORExt1718"}
#'   would query the 1718 database.
#' @param raw Logical, defaults to \code{FALSE}. Should the original tables
#'   from the database be returned? If \code{FALSE}, cleaned up names are
#'   returned and, for the \code{"Items"} table, item difficulties are
#'   returned instead of the full item attributes.
#' @param key Your personal key for accessing the Oregon Extended Database.
#'   Defaults to [db_key()].
#' @export

db_get <- function(table, db = NULL, raw = FALSE, key = db_key()) {
  check_tables(table)

  if (is.null(db)) {
    db <- current_db()
  }
  db <- check_db(db)
  txt_data <- get_raw_data(table, db, key)

  if (!raw & table == "Items") {
    out <- create_item_table(txt_data)
  } else if (nchar(txt_data) < 1) {
    out <- create_empty_frame(table, db)
  } else {
    out <- parse_txt_data(txt_data)
  }
  names(out) <- get_colnames(table, raw, db)
  out <- rm_rows_full_miss(out)

  if (is_tibble_installed()) {
    out <- tibble::as_tibble(out)
  }
  out
}
