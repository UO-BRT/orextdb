# Internal object storing the names of the database tables, which
# can be referenced in functions
tbls <- c(
  "Accomodations", "Answers", "Districts", "Exams", "Items",
  "Preferences", "Schools", "Students", "Students_old",
  "Submissions", "SupplementalDistricts", "SupplementalSchools",
  "Tasks", "User", "UserStudents", "UserStudents_old"
)

#' Function to check table name
#'
#' Stops R, returns an error, and lists available tables, if an invalid
#' table name is specified.
#'
#' @keywords internal
#' @noRd
#' @inheritParams db_get
#' @return Warning or nothing
#'
check_tables <- function(tbl) {
  if (!tbl %in% tbls) {
    stop("The table you requested is not part of the database.\nPlease request ",
         "one of the following tables:\n",
         paste0("* ", tbls, "\n"),
         call. = FALSE)
  }
}

# TALK TO EVAN ABOUT NEW FUNCTIONALITY
# WITH API TO LIST AVAILABLE DATABASES.
# THEN JUST TAKE THE NEWEST


#' Determines the current db based on the system date
#' Creates a string with the name of the db
#'
#' @noRd
#' @return Character vector — the most recent database name.
current_db <- function() {
  current_month <- as.numeric(format(Sys.Date(),"%m"))
  current_year <- as.numeric(format(Sys.Date(),"%y"))

  if (current_month < 9) {
    paste0("ORExt", paste0(current_year - 1, current_year))
  }

  else {
    paste0("ORExt", paste0(current_year, current_year + 1))
  }

}

#' Check user-specified database name and transforms it (if needed).
#' If the user improperly specified a database,
#' a message tells them how to fix it.
#'
#' @return Character vector — correctly formatted database name.
#' @inheritParams db_get
#' @export

check_db <- function(db) {
  if (is.null(db)) {
    return()
  }

  is_digits_only <- grepl("^\\d", db)

  is_four_digits <- nchar(db) == 4

  if (is_digits_only) {
    if (!is_four_digits) {
      explain_db_format(type = 'stop', reason = 'incorrect_num_digits')
    }
    if (is_four_digits) {
      explain_db_format(type = 'message', reason = 'digits_only')
    }
    db <- paste0("ORExt", db)
  }

  is_correct_format <- grepl("^ORExt\\d\\d\\d\\d$", db)

  if (!is_correct_format) {
    explain_db_format(type = 'stop', reason = 'incorrect_format')
  }
  year <- gsub("ORExt", "", db)
  y1 <- as.numeric(substr(year, 1, 2))
  y2 <- as.numeric(substr(year, 3, 4))

  if (y2 - y1 != 1) {
    explain_db_format(type = 'stop', reason = 'invalid_years')
  }
  db
}


#' Return the raw data from the api
#'
#' The raw data is returned as a single string. It then needs to be passed
#' to \code{\link{parse_txt_data}} or \code{\link{create_item_table}} (if
#' item data are being returned).
#'
#' @inheritParams db_get
#' @noRd
#' @keywords internal
#' @return Character vector — the raw data from the API

get_raw_data <- function(table, db, key) {
  tbl <- paste0(
    "https://orext.brtprojects.org/reportingAPIv1/tableDelimited?tableName=",
    table
  )
  tbl <- paste0(tbl, "&dbName=", db)
  hdr <- paste0(
    "Authorization: Bearer ",
    key
  )
  out <- GET(
    url = tbl,
    add_headers(Authorization = paste("Bearer", key))
  )
  txt <- rawToChar(out$content)
  enc2utf8(txt)
}

#' Convert text to data frame for items
#'
#' @param txt The raw text data returned from \code{\link{get_raw_data}}
#' @noRd
#' @keywords internal
#' @return data.frame - 4 columns: item_id, standard, brt_item_id, and item_difficulty.
create_item_table <- function(txt) {
  splt <- strsplit(txt, "\n")[[1]]
  splt <- lapply(splt, function(x) strsplit(x, "\t")[[1]])

  data.frame(
    item_id = vapply(splt, "[", 1, FUN.VALUE = character(1)),
    standard = vapply(splt, "[", 2, FUN.VALUE = character(1)),
    brt_item_id = toupper(vapply(splt, "[", 3, FUN.VALUE = character(1))),
    item_difficulty = vapply(splt, "[", 5, FUN.VALUE = character(1))
  )
}

#' Parse raw text data for all tables, except items
#' @param txt The raw text data returned from \code{\link{get_raw_data}}
#' @noRd
#' @keywords internal
#' @return data.frame - conversion of raw text data from API
parse_txt_data <- function(txt) {
  read.delim(
    text = txt,
    na.strings = c("NULL", "N/A"),
    header = FALSE
  )
}

#' Create empty data frame with correct column names.
#'
#' This function helps handle situations with no
#' data by returning an empty data frame with the
#' correct column names for the user-specified table.
#'
#' @inheritParams db_get
#' @noRd
#' @keywords internal
#' @return data.frame - contains column names that would be present for user-requested table

create_empty_frame <- function(table, db) {
  out <- vector("list", length(get_colnames(table, db = db)))
  out <- lapply(out, `c`, NA)
  as.data.frame(out, stringsAsFactors = FALSE)
}

#' Remove empty rows (i.e., those which are fully missing)
#' Any row in the data frame that is missing on all variables is removed
#'
#' @param d The data frame, typically the output from
#'   \code{\link{parse_txt_data}} or \code{\link{create_item_table}}
#' @noRd
#' @keywords internal
rm_empty_rows <- function(d) {
  full_missing <- apply(d, 1, function(x) sum(is.na(x)) == ncol(d))
  d[!full_missing, ]
}


#' Return the names in camelCase
#' @return A character vector of the new cleaned up names
#' @noRd
#' @keywords internal
#' @inheritParams db_get

return_camelCase_names <-
  function(table){

  switch(
    table,
    "Accomodations" = c("submissionID", "accomodation"),
    "Answers" =
      c("answerID", "taskID", "questionID", "score", "duration",
        "answerDate", "answer", "itemID", "positions", "isPractice",
        "isCorrect", "isFinal", "imageName"),
    "Districts" = c("districtID", "name"),
    "Exams" = c(
      "examID", "title", "form", "year"
      ),
    "Items" = c(
      "itemID", "standard", "brtItemID", "attrs"
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


#' Returns the column names for a given table
#'
#' This is a weird function and one that is likely to need updating over time.
#' As you can see below, the names depend on the year, because the tables don't
#' always have the same columns in each year. I had already written the main
#' function when I realized this so the rest of it is kind of hacked together
#' from that. You might want to consider redoing this so it calls separate
#' functions depending on the year, but there is a lot of overlap. Also new
#' tables are likely to be added to the database that might be relevant.
#'
#' @inheritParams db_get
#' @noRd
#' @keywords internal
#' @return Character vector - formatted column names in table

get_colnames <- function(table, raw, db) {
  if (!raw) {
    nms <- return_snake_case_names(table)
    if (db == "ORExt1819" & table == "Students") {
      nms <- nms[1:21]
    }
    if (db == "ORExt1718" & table == "Students") {
      nms <- nms[c(1:7, 9:14, 18:21)]
    }
    if (db == "ORExt1718" & (table == "Submissions" | table == "Answers")) {
      nms <- nms[-(length(nms) - 1)]
    }
    return(nms)
  }
  nms <- return_camelCase_names(table)

  if (db == "ORExt1819" & table == "Students") {
    nms <- nms[1:21]
  }
  if (db == "ORExt1718" & table == "Students") {
    nms <- nms[c(1:7, 9:14, 18:21)]
  }
  if (db == "ORExt1718" & table == "Submissions") {
    nms <- nms[-(length(nms) - 1)]
  }
  nms
}

#' Returns the column names for a given table in snake_case
#'
#' @return Character vector - formatted column names in table
#' @noRd
#' @keywords internal
#' @inheritParams db_get

return_snake_case_names <- function(table) {
  switch(table,
    "Accomodations" = c(
      "submission_id", "accomodation"
    ),
    "Answers" = c(
      "answer_id", "task_id", "question_id", "item_score", "duration",
      "answer_date", "answer", "item_id", "positions", "is_practice",
      "is_correct", "is_final", "image_name"
    ),
    "Districts" = c(
      "district_id", "name"
    ),
    "Exams" = c(
      "exam_id", "title", "form", "year"
    ),
    "Items" = c(
      "item_id", "standard", "item_id_brt", "item_difficulty"
    ),
    "Preferences" = c(
      "user_id", "name", "value"
    ),
    "Schools" = c(
      "district_id", "school_id", "name"
    ),
    "Students" = c(
      "student_id", "user_id", "fname", "mname", "lname", "nickname",
      "gender", "birth_date", "grade", "ssid", "new_ssid", "end_date",
      "district_id", "school_id", "res_dist_id", "res_sch_id",
      "dist_stdnt_id", "idea_elig_code1", "idea_elig_code2", "data_source",
      "date_ineligible", "hisp_eth_flg", "amer_ind_ak_ntv_flg",
      "asian_race_flg", "black_race_flg", "white_race_flg",
      "pac_isl_race_flg", "lang_origin", "econ_dsvnt_flg", "title1_flg",
      "sped_flg", "sect504_flg", "migrant_ed_flg", "indian_ed_flg", "el_flg",
      "distance_learn_flg", "homeschool_flg", "tag_potential",
      "tag_intel_gifted", "tag_reading", "tag_math", "tag_creative",
      "tag_leadership", "tag_perform_arts", "transition_prgm", "alted_flg",
      "amerind_tribal_mem", "amerind_tribal_enroll", "ethnic_cd"
      ),
    "Students_old" = c(
      "student_id", "user_id", "fname", "mname", "lname", "nickname",
      "gender", "birth_date", "grade", "ssid", "new_ssid", "end_date",
      "district_id", "school_id", "res_dist_id", "res_sch_id",
      "dist_stdnt_id", "idea_elig_code1", "idea_elig_code2", "data_source",
      "date_ineligible", "hisp_eth_flg", "amer_ind_ak_ntv_flg",
      "asian_race_flg", "black_race_flg", "white_race_flg",
      "pac_isl_race_flg", "lang_origin", "econ_dsvnt_flg", "title1_flg",
      "sped_flg", "sect504_flg", "migrant_ed_flg", "indian_ed_flg", "el_flg",
      "distance_learn_flg", "homeschool_flg", "tag_potential",
      "tag_intel_gifted", "tag_reading", "tag_math", "tag_creative",
      "tag_leadership", "tag_perform_arts", "transition_prgm", "alted_flg",
      "amerind_tribal_mem", "amerind_tribal_enroll", "ethnic_cd"
    ),
    "Submissions" = c(
      "submission_id", "student_id", "exam_id", "form", "date_started",
      "date_finished", "date_discont", "completed", "score", "num_correct",
      "num_attempt", "comment"
    ),
    "SupplementalDistricts" = c(
      "user_id", "district_id"
    ),
    "SupplementalSchools" = c(
      "user_id", "school_id"
    ),
    "Tasks" = c(
      "task_id", "submission_id", "task_type", "date_started",
      "date_finished", "completed", "score", "num_correct"
    ),
    "User" = c(
      "user_id", "user_name", "password", "user_type", "creds_verified",
      "email", "join_date", "district_id", "school_id", "fname", "lname",
      "is_dtc", "description"
    ),
    "UserStudents" = c(
      "student_id", "user_id", "date_added", "comment"
    ),
    "UserStudents_old" = c(
      "student_id", "user_id", "date_added", "comment"
    )
  )
}
