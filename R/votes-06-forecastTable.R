#' Prepare table with election forecast
#'
#' @param modelResults model results
#' @param data data
#' @param predDate prediction date
#'
#' @export
prepareForecastTable <- function(modelResults, data, predDate) {
  party_ids <- mapPartyNamesToNumbers(parties = data$parties)
  
  idxElectionDate <- which(data$timeSeq ==
                             floor(as.numeric(
                               difftime(
                                 as.Date(data$nextElectionDate),
                                 as.Date("1970-01-04"),
                                 units = "weeks"
                               )
                             )))
  forecastElection <-
    data.frame(
      date_forecast = predDate,
      type = "election_day",
      party_id = party_ids,
      cbind(
        as.matrix(modelResults$samples$yFinal[, , idxElectionDate] %>%
                    logistic %>%
                    colMeans) %>%
          round(4),
        apply(
          modelResults$samples$yFinal[, , idxElectionDate] %>%
            logistic,
          2,
          quantile,
          c(0.05, 0.95)
        ) %>%
          round(4) %>%
          t
      )
    )

  forecastNow <-
    data.frame(
      date_forecast = predDate,
      type = "next_sunday",
      party_id = party_ids,
      cbind(
        as.matrix(
          modelResults$samples$yFinal[, , 1 + which(data$timeSeq ==
                                                      floor(as.numeric(
                                                        difftime(as.Date(predDate),
                                                                 as.Date("1970-01-04"),
                                                                 units = "weeks")
                                                      )))] %>%
            logistic %>%
            colMeans %>%
            round(3)
        ),
        apply(
          modelResults$samples$yFinal[, , which(data$timeSeq ==
                                                  floor(as.numeric(
                                                    difftime(as.Date(predDate),
                                                             as.Date("1970-01-04"),
                                                             units = "weeks")
                                                  )))] %>%
            logistic,
          2,
          quantile,
          c(0.05, 0.95)
        ) %>%
          round(3) %>%
          t
      )
    )
  forecastTable <- rbind(forecastElection, forecastNow)
  names(forecastTable)[4:6] <-
    c("estimate", "lower_bound", "upper_bound")
  forecastTable$date_last_update <- Sys.time()
  return(forecastTable)
}

#' Prepare table with election forecast
#'
#' @param forecastData data with forecast
#'
#' @export
getForecastAllTable <- function(forecastData) {
  forecastData$date_forecast <- as.Date(forecastData$time)
  forecastData$lower_bound <- forecastData$lower
  forecastData$upper_bound <- forecastData$upper
  forecastData$upper_bound <- forecastData$upper
  parties <- mapPartyNamesToNumbers(parties = forecastData$party)
  forecastData$party_id <- parties
  forecastData$date_last_update <- Sys.time()
  forecastData[, c(
    "date_forecast",
    "party_id",
    "estimate",
    "lower_bound",
    "upper_bound",
    "date_last_update"
  )]
}

#' Calculate probs for events
#'
#' @param modelResults model results
#' @param data data
#' @param predDate prediction date
#'
#' @export
eventsDE <- function(modelResults, data, predDate) {
  electionForecast <-
    modelResults$samples$yFinal[, , 1 + which(data$timeSeq ==
                                                floor(as.numeric(
                                                  difftime(
                                                    as.Date(data$nextElectionDate),
                                                    as.Date("1970-01-04"),
                                                    units = "weeks"
                                                  )
                                                )))] %>%
    logistic
  electionForecast <-
    cbind(electionForecast, 1 - rowSums(electionForecast))
  colnames(electionForecast) <- c(data$parties, "Sonstige")
  electionForecast <- as.data.frame(electionForecast)
  #5% Rule
  bundestag <-
    electionForecast[, colnames(electionForecast) != "Sonstige"]
  bundestag[bundestag < 0.05] <- 0
  bundestag <- bundestag / rowSums(bundestag)
  bundestag <- as.data.frame(bundestag)
  eventData <- data.frame(
    date_forecast = predDate,
    event_id = c(1:15, 17:23),
    estimate = c((((
      bundestag %>%
        select("SPD", "GR\u00dcNE", "LINKE") %>%
        rowSums() > 0.5
    ) &
      (bundestag %>%
         select("LINKE") %>%
         rowSums() > 0) &
      (bundestag %>%
         select("GR\u00dcNE") %>%
         rowSums() > 0) &
      (bundestag %>%
         select("SPD") %>%
         rowSums() > 0)
    ) %>%
      sum) / nrow(bundestag),
    (((bundestag %>%
         select("CDU/CSU", "FDP") %>%
         rowSums() > 0.5) &
        (bundestag %>%
           select("FDP") %>%
           rowSums() > 0) &
        (bundestag %>%
           select("CDU/CSU") %>%
           rowSums() > 0)
    ) %>%
      sum) / nrow(bundestag),
    (((
      bundestag %>%
        select("CDU/CSU", "GR\u00dcNE") %>%
        rowSums() > 0.5
    ) &
      (bundestag %>%
         select("GR\u00dcNE") %>%
         rowSums() > 0) &
      (bundestag %>%
         select("CDU/CSU") %>%
         rowSums() > 0)
    ) %>%
      sum) / nrow(bundestag),
    (((bundestag %>%
         select("SPD", "GR\u00dcNE") %>%
         rowSums() > 0.5) &
        (bundestag %>%
           select("SPD") %>%
           rowSums() > 0) &
        (bundestag %>%
           select("GR\u00dcNE") %>%
           rowSums() > 0)
    ) %>%
      sum) / nrow(bundestag),
    (((
      bundestag %>%
        select("CDU/CSU", "GR\u00dcNE", "FDP") %>%
        rowSums() > 0.5
    ) &
      (bundestag %>%
         select("FDP") %>%
         rowSums() > 0) &
      (bundestag %>%
         select("GR\u00dcNE") %>%
         rowSums() > 0) &
      (bundestag %>%
         select("CDU/CSU") %>%
         rowSums() > 0)
    ) %>%
      sum) / nrow(bundestag),
    (((
      bundestag %>%
        select("SPD", "GR\u00dcNE", "FDP") %>%
        rowSums() > 0.5
    ) &
      (bundestag %>%
         select("FDP") %>%
         rowSums() > 0) &
      (bundestag %>%
         select("GR\u00dcNE") %>%
         rowSums() > 0) &
      (bundestag %>%
         select("SPD") %>%
         rowSums() > 0)
    ) %>%
      sum) / nrow(bundestag),
    (((bundestag %>%
         select("SPD", "CDU/CSU") %>%
         rowSums() > 0.5) &
        (bundestag %>%
           select("SPD") %>%
           rowSums() > 0) &
        (bundestag %>%
           select("CDU/CSU") %>%
           rowSums() > 0)
    ) %>%
      sum) / nrow(bundestag),
    ((
      bundestag %>%
        select("FDP") %>%
        rowSums() > 0
    ) %>%
      sum) / nrow(bundestag),
    ((
      bundestag %>%
        select("AfD") %>%
        rowSums() > 0
    ) %>%
      sum) / nrow(bundestag),
    ((
      electionForecast %>%
        select("CDU/CSU") %>%
        rowSums() > 0.4
    ) %>%
      sum) / nrow(electionForecast),
    ((
      electionForecast %>%
        select("SPD") %>%
        rowSums() > 0.3
    ) %>%
      sum) / nrow(electionForecast),
    ((
      electionForecast %>%
        select("SPD") %>%
        rowSums() < 0.2
    ) %>%
      sum) / nrow(electionForecast),
    ((
      electionForecast %>%
        select("SPD") %>%
        rowSums() > electionForecast %>%
        select("CDU/CSU") %>%
        rowSums()
    ) %>%
      sum
    ) / nrow(electionForecast),
    sum(
      apply(bundestag, 1, function(x)
        order(x, decreasing = TRUE)[3]) == which(colnames(bundestag) == "AfD")
    ) / nrow(bundestag),
    sum(apply(bundestag, 1, function(x)
      sum(x > 0) == 6)) / nrow(bundestag),
    sum((electionForecast %>%
           select("GR\u00dcNE") %>%
           rowSums()) > (electionForecast %>%
                           select("SPD") %>%
                           rowSums())
    ) / nrow(electionForecast),
    sum(
      apply(bundestag, 1, function(x)
        order(x, decreasing = TRUE)[1]) == which(colnames(bundestag) == "GR\u00dcNE")
    ) / nrow(bundestag),
    ((
      electionForecast %>%
        select("GR\u00dcNE") %>%
        rowSums() > 0.2
    ) %>%
      sum) / nrow(electionForecast),
    sum((electionForecast %>%
           select("LINKE") %>%
           rowSums()) > (electionForecast %>%
                           select("FDP") %>%
                           rowSums())
    ) / nrow(electionForecast),
    ((
      electionForecast %>%
        select("FDP") %>%
        rowSums() > 0.1
    ) %>%
      sum) / nrow(electionForecast),
    ((
      electionForecast %>%
        select("LINKE") %>%
        rowSums() > 0.1
    ) %>%
      sum) / nrow(electionForecast),
    ((
      bundestag %>%
        select("LINKE") %>%
        rowSums() > 0
    ) %>%
      sum) / nrow(bundestag)
    )
  )
  return(eventData[1:22, ])
}

#' Calculate part of government probabilities
#'
#' @param fact_coalition_prob coalition probs
#' @param predDate prediction date
#'
#' @export
partOfGovernmentDE <- function(fact_coalition_prob, predDate) {
  part_of_government_probs <- data.frame(
    date_forecast = predDate,
    party = c("CDU/CSU", "SPD", "AfD", "GR\u00dcNE", "LINKE", "FDP")
  )
  parties <- mapPartyNamesToNumbers(parties =
                                      part_of_government_probs$party)
  part_of_government_probs$party_id <- parties
  
  coalitions <-
    strsplit(fact_coalition_prob$possible_coalition, "-")
  
  part_of_government_probs$estimate <-
    sapply(part_of_government_probs$party, function(party) {
      coalitions_containing_party <-
        sapply(coalitions, function(coalition) {
          party %in% coalition
        })
      sum(fact_coalition_prob[coalitions_containing_party, ]$estimate)
    })
  part_of_government_probs$party <- NULL
  part_of_government_probs
}

#' Prepare survey data
#'
#' @param pollData data frame with survey data
#'
#' @export
getFactSurvey <- function(pollData) {
  fact_survey <-
    gather(pollData %>%
             select(-c("Befragte", "Sonstige")),
           key = "parties",
           value = "estimate", -Datum, -Institut)
  
  parties <- mapPartyNamesToNumbers(parties = fact_survey$parties)
  fact_survey$party_id <- parties
  fact_survey$parties <- NULL
  fact_survey <-
    fact_survey[, c("Datum", "Institut", "party_id", "estimate")]
  fact_survey <- na.omit(fact_survey)
  fact_survey$Institut[fact_survey$Institut == "Infratestdimap"] <-
    "Infratest dimap"
  fact_survey$Institut[fact_survey$Institut == "Verian(Emnid)"] <-
    "Verian (Emnid)"
  fact_survey$Institut[grepl("Forsch", fact_survey$Institut)] <-
    "Forschungsgruppe Wahlen"
  fact_survey
}
