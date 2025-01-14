#' Get index of a date
#' 
#' @description
#' Get the index of the entry of a sequence that is equal to the number of weeks
#' between 1970-01-04 and the given date
#' 
#' @param timeSeq Vector of numbers
#' @param date a date in the format "YYYY-mm-dd"
#' @return numeric, an index
getDateIdInTimeseq <- function(timeSeq, date) {
  nWeeks <- weeksBetweenOriginAndDate(date)
  # Check where timeSeq has exactly this value
  which(timeSeq == nWeeks)
}


#' Weeks between 1970-01-04 and a given date
#' 
#' @description
#' Compute the number of weeks between 1970-01-04 and the given date
#' Only completed weeks count; e.g., if it's 1 week and 6 days, the result is
#' 1 (week)
#' 
#' @param date a date in the format "YYYY-mm-dd"
#' @return numeric
weeksBetweenOriginAndDate <- function(date) {
  date <- as.Date(date)
  originDate <- as.Date("1970-01-04")
  timeDiff <- difftime(date, originDate, units = "weeks")
  nWeeks <- floor(as.numeric(timeDiff))
}

#' @title Fill up gaps between dates with data points from the most recent event
#'
#' @description All date gaps are filled up. This translates into the creation
#' of new rows for each coalition event and a percent estimation value equal to
#' the most recent
#'
#' @param df Incomplete dataframe with percent estimates for events
#' @returns Time-consistent dataframe
#' @export
fillDateGapsPerEvent <- function(df) {
  
  df_filled <- df %>%
    group_by(.data$event_id) %>%
    arrange(.data$date_forecast, .by_group = TRUE) %>%
    mutate(date_forecast = as.Date(.data$date_forecast)) %>%
    complete(date_forecast = seq(min(.data$date_forecast), max(.data$date_forecast), by = "days")) %>%
    fill(everything(), .direction = "down") %>%
    ungroup() %>%
    mutate(date_forecast = as.character(.data$date_forecast)) %>%
    select(date_forecast, everything())
  
  return(df_filled)
}
