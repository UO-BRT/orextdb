#' Function to query the live OR Extended database and return specific tables
#'
#' @param table A string specifying the specific table from the Oregon Extended
#'   live database. Should be one of "Accomodations", "Answers", "Districts",
#'   "Exams", "Items", "Preferences", "Schools", "Students", "Students_old",
#'   "Submissions", "SupplementalDistricts", "SupplementalSchools", "Tasks",
#'   "User", "UserStudents", or "UserStudents_old".
#' @param db A string specifying the database to query. Defaults to 
#'  \code{NULL}, in which case teh most recent database is queried. These
#'   names should be specified as in the database, e.g., \code{"ORExt1920"}
#'   would query the 1920 database.
#' @param raw Logical, defaults to \code{FALSE}. Should the original tables
#'   from the database be returned? If \code{FALSE}, cleaned up names are
#'   returned and, for the \code{"Items"} table, item difficulties are
#'   returned instead of the full item attributes.
#' @param key Your personal key for accessing the Oregon Extended Database.
#'   Defaults to [db_key()].
#' @export
#'

db_get <- function(table, db = NULL, raw = FALSE, key = db_key()) {
  check_tables(table)
  db <- check_db(db)

  tbl <- paste0(
    "https://orext.brtprojects.org/reportingAPIv1/tableDelimited?tableName=",
    table
  )
  if (!is.null(db)) {
    tbl <- paste0(tbl, "&dbName=", db)
  }
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

  if (!raw & table == "Items") {
    splt <- strsplit(txt, "\n")[[1]]
    splt <- lapply(splt, function(x) strsplit(x, "\t")[[1]])
    json <- vapply(splt, "[", 4, FUN.VALUE = character(1))

   out <- data.frame(
     item_id = vapply(splt, "[", 1, FUN.VALUE = character(1)),
     standard = vapply(splt, "[", 2, FUN.VALUE = character(1)),
     brt_item_id = vapply(splt, "[", 3, FUN.VALUE = character(1)),
     item_difficulty = get_difficulties(convert_json(json))
    )

  } else if (nchar(txt) < 1) {
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
  names(out) <- get_colnames(table, raw)

  # remove rows with full missing data
  full_missing <- apply(out, 1, function(x) sum(is.na(x)) == ncol(out))
  out <- out[!full_missing, ]

  if(is_tibble_installed()) {
    out <- tibble::as_tibble(out)
  }
  out
}

get_colnames <- function(table, raw = FALSE) {
  if (!raw) {
    return(swap_colnames(table))
  }
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

#' Function to clean up the names of an important table
#'
#' @param table A string specifying the specific table from the Oregon Extended
#'   live database. Should be one of "Accomodations", "Answers", "Districts",
#'   "Exams", "Items", "Preferences", "Schools", "Students", "Students_old",
#'   "Submissions", "SupplementalDistricts", "SupplementalSchools", "Tasks",
#'   "User", "UserStudents", or "UserStudents_old".
#' @return A character vector of the new cleaned up names
#' @noRd
#' @keywords internal

swap_colnames <- function(table) {
  switch(
    table,
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
