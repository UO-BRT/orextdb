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
  txt <- enc2utf8(txt)

  if (nchar(txt) < 1) {
    out <- vector("list", length(get_colnames(table)))
    out <- lapply(out, `c`, NA)
    out <- as.data.frame(out, stringsAsFactors = FALSE)
  } else {
    out <- read.delim(
      text = txt,
      na.strings = c("NULL", "N/A"),
      header = FALSE
    )
  }
  names(out) <- get_colnames(table)

  # remove rows with full missing data
  full_missing <- apply(out, 1, function(x) sum(is.na(x)) == ncol(out))
  out <- out[!full_missing, ]

  if(is_tibble_installed()) {
    out <- tibble::as_tibble(out)
  }
  out
}

get_colnames <- function(table) {
  switch(
    table,
    "Accomodations" = c(
      "submissionID", "accomodation"
    ),
    "Answers" = c(
      "answerID", "taskID", "questionID", "score", "duration",
      "answerDate", "answer", "itemID", "positions", "isPractice",
      "isCorrect", "isFinal", "imageName"
    ),
    "Districts" = c(
      "districtID", "name"
    ),
    "Exams" = c(
      "examID", "title", "form", "year"
    ),
    "Items" = c(
      "itemID", "standard", "brtItemID"
    ),
    "Preferences" = c(
      "userID", "name", "value"
    ),
    "Schools" = c(
      "districtID", "schoolID", "name"
    ),
    "Students" = c(
      "studentID", "userID", "fname", "mname", "lname", "nickname",
      "gender", "birthDate", "grade", "SSID", "newSSID", "endDt",
      "districtID", "schoolID", "resDistID", "resSchID",
      "distStdntID", "ideaEligCode1", "ideaEligCode2", "dataSource",
      "dateIneligible", "HispEthnicFg", "AmerIndianAlsknNtvRaceFg",
      "AsianRaceFg", "BlackRaceFg", "WhiteRaceFg", "PacIsIndrRaceFg",
      "LangOrgn", "EconDsvntgFg", "Ttl1Fg", "SpEdFg", "Sect504Fg",
      "MigrntEdFg", "IndianEdFg", "ELFg", "DstncLrnFg", "HomeSchlFg",
      "TAGPtntTAGFg", "TAGIntlctGiftFg", "TAGAcdmTlntRdFg", "TAGAcdmTlntMaFg",
      "TAGCrtvAbltyFg", "TAGLdrshpAbltyFg", "TAGPrfmArtsAbltyFg",
      "TrnstnProgFg", "AltEdProgFg", "AmerIndianTrbMbrshpCd",
      "AmerIndianTrbEnrlmntNbr", "EthnicCd"
    ),
    "Students_old" = c(
      "studentID", "userID", "fname", "mname", "lname", "nickname",
      "gender", "birthDate", "grade", "SSID", "newSSID", "endDt",
      "districtID", "schoolID", "resDistID", "resSchID",
      "distStdntID", "ideaEligCode1", "ideaEligCode2", "dataSource",
      "dateIneligible", "HispEthnicFg", "AmerIndianAlsknNtvRaceFg",
      "AsianRaceFg", "BlackRaceFg", "WhiteRaceFg", "PacIsIndrRaceFg",
      "LangOrgn", "EconDsvntgFg", "Ttl1Fg", "SpEdFg", "Sect504Fg",
      "MigrntEdFg", "IndianEdFg", "ELFg", "DstncLrnFg", "HomeSchlFg",
      "TAGPtntTAGFg", "TAGIntlctGiftFg", "TAGAcdmTlntRdFg", "TAGAcdmTlntMaFg",
      "TAGCrtvAbltyFg", "TAGLdrshpAbltyFg", "TAGPrfmArtsAbltyFg",
      "TrnstnProgFg", "AltEdProgFg", "AmerIndianTrbMbrshpCd",
      "AmerIndianTrbEnrlmntNbr", "EthnicCd"
    ),
    "Submissions" = c(
      "submissionID", "studentID", "examID", "form", "dateStarted",
      "dateFinished", "dateDiscontinued", "completed", "score", "numCorrect",
      "numAttempted", "comment"
    ),
    "SupplementalDistricts" = c(
      "userID", "districtID"
    ),
    "SupplementalSchools" = c(
      "userID", "schoolID"
    ),
    "Tasks" = c(
      "taskID", "submissionID", "taskType", "dateStarted",
      "dateFinished", "completed", "score", "numCorrect"
    ),
    "User" = c(
      "userID", "username", "password", "user_type", "creds_verified",
      "email", "join_date", "districtID", "schoolID", "fname", "lname",
      "is_dtc", "description"
    ),
    "UserStudents" = c(
      "studentID", "userID", "dateAdded", "comment"
    ),
    "UserStudents_old" = c(
      "studentID", "userID", "dateAdded", "comment"
    )
  )
}
