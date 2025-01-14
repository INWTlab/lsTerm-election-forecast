#' Throw error if there are new pollsters
#'
#' @description
#' If www.wahlrecht.de delivers a pollster (institute) that is not known in the
#' db, this leads to empty institute values in the database and some data will
#' be ignored. This function throws an error if there is a new institute.
#'
#' @param pollstersWeb character vector with pollsters from wahlrecht.de
#' @param pollstersDb character vector with pollsters form db
#' @export
stopIfNewPollster <- function(pollstersWeb, pollstersDb) {
  newPollsters <- setdiff(pollstersWeb, pollstersDb)
  if (length(newPollsters) > 0) {
    message <- paste0(
      "There are new pollsters in the data: ", newPollsters,
      "\nPlease check if an institute has been renamed or a new institute has been added."
    )
    stop(message)
  }
}


#' Check if there are new polls
#'
#' @description
#' Check if there is a newer poll available on the web than in the database
#' for any pollster.
#'
#' @param latestPolls data.frame with columns "Institut" and "latestPoll" as
#' returned from latestPollPerPollster()
#' @param latestPollsDb data.frame with columns "Institut" and "latestPollDb" as
#' returned from getLatestPollsDb()
#' @return logical
#'
#' @export
checkForNewPolls <- function(latestPolls, latestPollsDb) {
  pollDates <- left_join(latestPollsDb, latestPolls, by = "Institut")
  return(any(pollDates$latestPollDb < pollDates$latestPoll))
}


#' Check if data was changed
#'
#' @description
#' Check if old poll data was changed after a pollster published new data.
#'
#' @param polls data.frame scraped from wahlrecht.de. Reaches back a whole year.
#' @param cred credentials specified in scriptMulti.R
#' @return logical
#'
#' @export
checkIfPollDataChanged <- function(polls, cred) {
  pollsDB <- sendQuery(
    db = cred,
    "SELECT * FROM fact_survey WHERE Datum >= DATE_SUB((SELECT MAX(Datum) FROM fact_survey), INTERVAL 52 WEEK) ORDER BY Datum DESC;"
  )
  pollsDB$Datum <- as.Date(pollsDB$Datum)
  polls <- polls[polls$Datum >= as.Date(min(pollsDB$Datum)), ]
  mismatches <- setdiff(pollsDB, polls)
  if (nrow(mismatches) > 0) {
    cat("A mismatch occured in the following lines:\n")
    print(mismatches)
    stop()
  }
  cat("So far, nothing has changed.\n")
}


#' Date of latest poll per pollster
#'
#' @param fact_survey data.frame with columns "Institut" and "Datum"
#'
#' @export
latestPollPerPollster <- function(fact_survey) {
  fact_survey %>%
    group_by(Institut) %>%
    summarise(latestPoll = max(Datum)) %>%
    arrange(desc(latestPoll))
}


#' Get date of latest poll per pollster in the database
#' @param cred DB credentials
#' @return data.frame with columns "Institut" and "latestPollDb"
#' @export
getLatestPollsDb <- function(cred) {
  sendQuery(db = cred,
            "select Institut, max(Datum) as latestPollDb
                                    from fact_survey group by Institut;")
}
