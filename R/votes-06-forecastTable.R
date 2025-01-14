#' Prepare table with election forecast
#'
#' @param modelResults model results
#' @param data data
#' @param predDate prediction date
#'
#' @export
prepareForecastTable <- function(modelResults, data, predDate) {
  party_ids <- mapPartyNamesToNumbers(parties = data$parties)
  
  idxElectionDate <-
    getDateIdInTimeseq(data$timeSeq, data$nextElectionDate)
  resultsElectionDate <-
    modelResults$samples$yFinal[, , idxElectionDate]
  forecastElection <-
    data.frame(
      date_forecast = predDate,
      type = "election_day",
      party_id = party_ids,
      cbind(
        # Point estimate
        as.matrix(resultsElectionDate %>% logistic %>% colMeans) %>% round(4),
        # Credible intervals
        apply(resultsElectionDate %>% logistic, 2, quantile, c(0.05, 0.95)) %>%
          round(4) %>% t
      )
    )
  
  idxNow <- getDateIdInTimeseq(data$timeSeq, as.Date(predDate))
  resultsNow <- modelResults$samples$yFinal[, , idxNow]
  forecastNow <-
    data.frame(
      date_forecast = predDate,
      type = "next_sunday",
      party_id = party_ids,
      cbind(
        # Point estimate
        as.matrix(resultsNow %>% logistic %>% colMeans %>% round(3)),
        # Credible intervals
        apply(resultsNow %>% logistic, 2, quantile, c(0.05, 0.95)) %>%
          round(3) %>% t
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
    modelResults$samples$yFinal[, , getDateIdInTimeseq(data$timeSeq,
                                                       as.Date(data$nextElectionDate))] %>%
    logistic
  electionForecast <-
    cbind(electionForecast, 1 - rowSums(electionForecast))
  colnames(electionForecast) <- c(data$parties, "Sonstige")
  electionForecast <- as.data.frame(electionForecast)
  
  # 5% Rule
  bundestag <-
    electionForecast[, colnames(electionForecast) != "Sonstige"]
  bundestag[bundestag < 0.05] <- 0
  bundestag <- bundestag / rowSums(bundestag)
  bundestag <- as.data.frame(bundestag)

  fdp_in_bundestag <- bundestag[, "FDP"] > 0
  cdu_in_bundestag <- bundestag[, "CDU/CSU"] > 0
  gruene_in_bundestag <- bundestag[, "GR\u00dcNE"] > 0
  spd_in_bundestag <- bundestag[, "SPD"] > 0
  linke_in_bundestag <- bundestag[, "LINKE"] > 0
  bsw_in_bundestag <- bundestag[, "BSW"] > 0
  
  maj_cdu_fdp <-
    bundestag %>% select("CDU/CSU", "FDP") %>% rowSums() > 0.5
  maj_cdu_gruen <-
    bundestag %>% select("CDU/CSU", "GR\u00dcNE") %>% rowSums() > 0.5
  maj_spd_gruen <-
    bundestag %>% select("SPD", "GR\u00dcNE") %>% rowSums() > 0.5
  maj_jamaica <-
    bundestag %>% select("CDU/CSU", "GR\u00dcNE", "FDP") %>% rowSums() > 0.5
  maj_traffic_light <-
    bundestag %>% select("SPD", "GR\u00dcNE", "FDP") %>% rowSums() > 0.5
  maj_grand <-
    bundestag %>% select("SPD", "CDU/CSU") %>% rowSums() > 0.5
  maj_kenia <-
    bundestag %>% select("SPD", "CDU/CSU", "GR\u00dcNE") %>% rowSums() > 0.5
  maj_germany <-
    bundestag %>% select("SPD", "CDU/CSU", "FDP") %>% rowSums() > 0.5
  maj_cdu_gruen_bsw <-
    bundestag %>% select("CDU/CSU", "GR\u00dcNE", "BSW") %>% rowSums() > 0.5
  maj_cdu_spd_bsw <-
    bundestag %>% select("CDU/CSU", "SPD", "BSW") %>% rowSums() > 0.5
  
  eventData <- data.frame(
    date_forecast = predDate,
    event_id = c(3, 5, 6, 7, 8, 10, 17, 20, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32),
    estimate = c(
      # 3: Majority for black-green
      ((maj_cdu_gruen & gruene_in_bundestag & cdu_in_bundestag)
       %>% sum) / nrow(bundestag),
      # 5: Majority for "Jamaica" (black-green-yellow)
      ((maj_jamaica & fdp_in_bundestag & gruene_in_bundestag & cdu_in_bundestag )
       %>% sum ) / nrow(bundestag),
      # 6: Majority for "traffic light" (red-yellow-green)
      (( maj_traffic_light & fdp_in_bundestag & gruene_in_bundestag & spd_in_bundestag )
       %>% sum ) / nrow(bundestag),
      # 7: Majority for grand coalition
      ((maj_grand & spd_in_bundestag & cdu_in_bundestag)
       %>% sum) / nrow(bundestag),
      # 8: FDP gets elected to the Bundestag
      sum(fdp_in_bundestag) / nrow(bundestag),
      # 10: CDU/CSU having more than 40 percent of the votes
      ((electionForecast %>% select("CDU/CSU") %>% rowSums() > 0.4 )
       %>% sum) / nrow(electionForecast),
      # 17: Grüne stronger than SPD
      sum((electionForecast %>% select("GR\u00dcNE") %>% rowSums()) >
            (electionForecast %>% select("SPD") %>% rowSums())
      ) / nrow(electionForecast),
      # 20: Linke having more votes than FDP
      sum((electionForecast %>% select("LINKE") %>% rowSums()) >
            (electionForecast %>% select("FDP") %>% rowSums())
      ) / nrow(electionForecast),
      # 23: Linke gets elected to the Bundestag
      sum(linke_in_bundestag) / nrow(bundestag),
      # 24: AfD becomes second strongest faction
      sum(apply(bundestag, 1, function(x)
        order(x, decreasing = TRUE)[2]) == which(colnames(bundestag) == "AfD")) / nrow(bundestag),
      # 25: SPD having more than 20 percent of the votes
      ((electionForecast %>% select("SPD") %>% rowSums() > 0.2)
       %>% sum) / nrow(electionForecast),
      # 26: Majority for Kenia coalition
      ((maj_kenia & spd_in_bundestag & cdu_in_bundestag & gruene_in_bundestag)
       %>% sum) / nrow(bundestag),
      # 27: Majority for Germany coalition
      ((maj_germany & spd_in_bundestag & cdu_in_bundestag & fdp_in_bundestag)
       %>% sum) / nrow(bundestag),
      # 28: BSW gets elected to the Bundestag
      sum(bsw_in_bundestag) / nrow(bundestag),
      # 29: CDU becomes strongest faction
      sum(apply(bundestag, 1, function(x)
        order(x, decreasing = TRUE)[1]) == which(colnames(bundestag) == "CDU/CSU")) / nrow(bundestag),
      # 30: Grüne gets elected to the Bundestag
      sum(gruene_in_bundestag) / nrow(bundestag),
      # 31: Majority for black-green-orange
      ((maj_cdu_gruen_bsw & cdu_in_bundestag & gruene_in_bundestag & bsw_in_bundestag)
        %>% sum) / nrow(bundestag),
      # 32: Majority for black-red-orange
      ((maj_cdu_spd_bsw & cdu_in_bundestag & spd_in_bundestag & bsw_in_bundestag)
       %>% sum) / nrow(bundestag)
    )
  )
  return(eventData)
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
    party = c("CDU/CSU", "SPD", "AfD", "GR\u00dcNE", "LINKE", "FDP", "BSW")
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
