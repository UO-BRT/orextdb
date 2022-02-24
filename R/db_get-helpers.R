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

# CHRIS FIX
# This function won't always work - need to know if it's at start or end of year
# e.g., at the start of next year we would want to add one to the year, not
# subtract one (use the month for a cutoff)

# Actually - check in with Evan for new functionality in the API that will list
# all the dbs, then just take the most recent one.

#' Used in case the db is not passed to a function
#' Determines the current db based on the system date
#' Creates a string with the name of the db
#' @noRd
#' @keywords internal
current_db <- function() {
  year <- as.numeric(gsub("^\\d\\d(\\d\\d).+", "\\1", Sys.Date()))
  paste0("ORExt", paste0(year - 1, year))
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


#' Returns the raw data from the api
#'
#' The raw data is returned as a single string. It then needs to be passed
#' to \code{\link{parse_txt_data}} or \code{\link{create_item_table}} (if
#' item data are being returned).
#' @inheritParams db_get
#' @noRd
#' @keywords internal
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

#' Parses the raw text data for items
#' @param txt The raw text data returned from \code{\link{get_raw_data}}
#' @noRd
#' @keywords internal
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

#' Parses the raw text data for all tables except items
#' @param txt The raw text data returned from \code{\link{get_raw_data}}
#' @noRd
#' @keywords internal
parse_txt_data <- function(txt) {
  read.delim(
    text = txt,
    na.strings = c("NULL", "N/A"),
    header = FALSE
  )
}

#' Returns an empty data frame w/correct column names
#'
#' Occassionally there is no data. This function helps handle that by returning
#' an empty data frame with the correct column names for the given table.
#'
#' @inheritParams db_get
#' @noRd
#' @keywords internal
create_empty_frame <- function(table, db) {
  out <- vector("list", length(get_colnames(table, db = db)))
  out <- lapply(out, `c`, NA)
  as.data.frame(out, stringsAsFactors = FALSE)
}

#' Removes fully missing rows
#' Any row in the data frame that is missing on all columns is removed
#' @param d The data frame, typically the output from
#'   \code{\link{parse_txt_data}} or \code{\link{create_item_table}}
#' @noRd
#' @keywords internal
rm_rows_full_miss <- function(d) {
  full_missing <- apply(d, 1, function(x) sum(is.na(x)) == ncol(d))
  d[!full_missing, ]
}

#' Returns the column names for a given table
#'
#' This is a weird function and one that is likely to need updating over time.
#' As you can see below, the names depend on the year, because the tables don't
#' always have the same columns in each year. I had already written the main
#' function when I realized this so the rest of it is kind of hacked together
#' from that.You might want to consider redoing this so it calls separate
#' functions depending on the year, but there is a lot of overlap. Also new
#' tables are likely to be added to the database that might be relevant.
#'
#' @inheritParams db_get
#' @noRd
#' @keywords internal
get_colnames <- function(table, raw, db) {
  if (!raw) {
    nms <- swap_colnames(table)
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
  nms <- switch(table,
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

#' Returns the column names for a given function
#'
#' This is just like \code{\link{get_colnames}} except it returns the raw
#' names as they are in the database, rather than cleaning them up a bit
#' (e.g., moving from camelCase to snake_case).
#'
#' @return A character vector of the new cleaned up names
#' @noRd
#' @keywords internal

swap_colnames <- function(table) {
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
