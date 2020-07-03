getForecastTable <- function(modelResults, data, predDate){

  forecastElection <- data.frame(date_forecast = predDate, type = "election_day", party_id = data$parties,
                                 cbind(as.matrix(modelResults$samples$y[,,1 + which(data$timeSeq == 
                    floor(as.numeric(difftime(as.Date(data$nextElectionDate),
                                              as.Date("1970-01-04"),
                                              units = "weeks"))))] %>%
  logistic %>% colMeans) %>% round(3), apply(modelResults$samples$y[,,1 + which(data$timeSeq == 
                                                                 floor(as.numeric(difftime(as.Date(data$nextElectionDate),
                                                                                          as.Date("1970-01-04"),
                                                                                          units = "weeks"))))] %>%
                                 logistic, 2, quantile, c(0.025, 0.975)) %>% round(3) %>% t))
  
  forecastNow <- data.frame(date_forecast = predDate, type = "next_sunday", party_id = data$parties, cbind(as.matrix(modelResults$samples$y[,,which(data$timeSeq == 
                                                                             floor(as.numeric(difftime(as.Date(predDate),
                                                                                                       as.Date("1970-01-04"),
                                                                                                       units = "weeks"))))] %>%
                                              logistic %>% colMeans %>% round(3)), apply(modelResults$samples$y[,,which(data$timeSeq == 
                                                                                                             floor(as.numeric(difftime(as.Date(predDate),
                                                                                                                                       as.Date("1970-01-04"),
                                                                                                                                       units = "weeks"))))] %>%
                                                                              logistic, 2, quantile, c(0.025, 0.975)) %>% round(3) %>% t))
  forecastTable <- rbind(forecastElection, forecastNow)
  names(forecastTable)[4:6] <- c("estimate", "lower_bound", "upper_bound")
  forecastTable$date_last_update <- Sys.time()
  return(forecastTable)
}

eventsDE <- function(modelResults, data, predDate){
  electionForecast <- modelResults$samples$y[,,1 + which(data$timeSeq == 
                                                       floor(as.numeric(difftime(as.Date(data$nextElectionDate),
                                                                                 as.Date("1970-01-04"),
                                                                                 units = "weeks"))))] %>% logistic
  electionForecast <- cbind(electionForecast, 1 - rowSums(electionForecast))
  colnames(electionForecast) <- c(data$parties, "Sonstige")
  electionForecast <- as.data.frame(electionForecast)
  #5% Rule
  bundestag <- electionForecast[, colnames(electionForecast) != "Sonstige"]
  bundestag[bundestag < 0.05] <- 0
  bundestag <- bundestag / rowSums(bundestag)
  bundestag <- as.data.frame(bundestag)
  data.frame(date_forecast = predDate,
             event = c("Mehrheit für Rot-Rot-Grün",
                       "Mehrheit für Schwarz-Gelb",
                       "Mehrheit für Schwarz-Grün",
                       "Mehrheit für Rot-Grün",
                       "Mehrheit für \"Jamaika\" (Schwarz-Grün-Gelb)",
                       "Mehrheit für \"Ampel\" (Rot-Gelb-Grün)",
                       "Mehrheit für große Koalition",
                       "FDP kommt in den Bundestag",
                       "AfD kommt in den Bundestag",
                       "CDU/CSU bekommt mehr als 40% der Stimmen",
                       "SPD bekommt mehr als 30% der Stimmen",
                       "SPD bekommt weniger als 20% der Stimmen",
                       "SPD stärker als CDU/CSU",
                       "AfD wird drittstärkste Fraktion",
                       "Sechs Fraktionen im Bundestag vertreten"),
             estimate = c(
               (bundestag %>% filter((SPD + GRÜNE + LINKE) > 0.5) %>% nrow) / nrow(bundestag),
               ((bundestag %>% select("CDU/CSU", "FDP") %>% rowSums() > 0.5) %>% sum) / nrow(bundestag),
               ((bundestag %>% select("CDU/CSU", "GRÜNE") %>% rowSums() > 0.5) %>% sum) / nrow(bundestag),
               ((bundestag %>% select("SPD", "GRÜNE") %>% rowSums() > 0.5) %>% sum) / nrow(bundestag),
               ((bundestag %>% select("CDU/CSU", "GRÜNE", "FDP") %>% rowSums() > 0.5) %>% sum) / nrow(bundestag),
               ((bundestag %>% select("SPD", "GRÜNE", "FDP") %>% rowSums() > 0.5) %>% sum) / nrow(bundestag),
               ((bundestag %>% select("SPD", "CDU/CSU") %>% rowSums() > 0.5) %>% sum) / nrow(bundestag),
               ((bundestag %>% select("FDP") %>% rowSums() > 0) %>% sum) / nrow(bundestag),
               ((bundestag %>% select("AfD") %>% rowSums() > 0) %>% sum) / nrow(bundestag),
               ((electionForecast %>% select("CDU/CSU") %>% rowSums() > 0.4) %>% sum) / nrow(electionForecast),
               ((electionForecast %>% select("SPD") %>% rowSums() > 0.3) %>% sum) / nrow(electionForecast),
               ((electionForecast %>% select("SPD") %>% rowSums() < 0.2) %>% sum) / nrow(electionForecast),
               ((electionForecast %>% select("SPD") %>% rowSums() > electionForecast %>% select("CDU/CSU") %>% rowSums()) %>% sum) / nrow(electionForecast),
               sum(apply(bundestag, 1, function(x) order(x)[3]) == which(colnames(bundestag) == "AfD")) / nrow(bundestag),
               sum(apply(bundestag, 1, function(x) sum(x > 0) == 6)) / nrow(bundestag)
               )
             )
}