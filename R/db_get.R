#' Function to query the live OR Extended database and return specific tables
#'
#'@param table A string specifying the specific table from the Oregon Extended
#'  live database. Should be one of "Accomodations", "Answers", "Districts",
#'  "Exams", "Items", "Preferences", "Schools", "Students", "Students_old",
#'  "Submissions", "SupplementalDistricts", "SupplementalSchools", "Tasks",
#'  "User", "UserStudents", or "UserStudents_old".
#'@param key Your personal key for accessing the Oregon Extended Database.
#'  Defaults to [db_key()].
#'@export
#'
db_get <- function(table, key = db_key()) {
  check_tables(table)
  tbl <- paste0(
    "https://orext.brtprojects.org/reportingAPIv1/tableDelimited?tableName=",
    table
  )
  hdr <- paste0(
    "Authorization: Bearer ",
    key
  )
  out <- GET(
    url = tbl,
    add_headers(Authorization = paste("Bearer", key))
  )
  txt <- rawToChar(out$content)
  out <- read.table(
    text = txt,
    sep = "\t",
    na.strings = c("NULL", "N/A")
  )

  if(is_tibble_installed()) {
    out <- tibble::as_tibble(out)
  }
  out
}

