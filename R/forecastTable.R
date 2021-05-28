#' @export
getForecastTable <- function(modelResults, data, predDate){
  
  parties <- data$parties
  parties[parties == "CDU/CSU"] <- 1
  parties[parties == "SPD"] <- 2
  parties[parties == "AfD"] <- 3
  parties[parties == "GRÜNE"] <- 4
  parties[parties == "LINKE"] <- 5
  parties[parties == "FDP"] <- 6
  parties <- as.numeric(parties)
  
  forecastElection <- data.frame(date_forecast = predDate, type = "election_day", party_id = parties,
                                 cbind(as.matrix(modelResults$samples$yFinal[,,1 + which(data$timeSeq == 
                                                                                           floor(as.numeric(difftime(as.Date(data$nextElectionDate),
                                                                                                                     as.Date("1970-01-04"),
                                                                                                                     units = "weeks"))))] %>%
                                                   logistic %>% colMeans) %>% round(3), apply(modelResults$samples$yFinal[,,1 + which(data$timeSeq == 
                                                                                                                                        floor(as.numeric(difftime(as.Date(data$nextElectionDate),
                                                                                                                                                                  as.Date("1970-01-04"),
                                                                                                                                                                  units = "weeks"))))] %>%
                                                                                                logistic, 2, quantile, c(0.05, 0.95)) %>% round(3) %>% t))
  
  forecastNow <- data.frame(date_forecast = predDate, type = "next_sunday", party_id = parties, cbind(as.matrix(modelResults$samples$yFinal[,,1 + which(data$timeSeq == 
                                                                                                                                                          floor(as.numeric(difftime(as.Date(predDate),
                                                                                                                                                                                    as.Date("1970-01-04"),
                                                                                                                                                                                    units = "weeks"))))] %>%
                                                                                                                  logistic %>% colMeans %>% round(3)), apply(modelResults$samples$yFinal[,,which(data$timeSeq == 
                                                                                                                                                                                                   floor(as.numeric(difftime(as.Date(predDate),
                                                                                                                                                                                                                             as.Date("1970-01-04"),
                                                                                                                                                                                                                             units = "weeks"))))] %>%
                                                                                                                                                               logistic, 2, quantile, c(0.05, 0.95)) %>% round(3) %>% t))
  forecastTable <- rbind(forecastElection, forecastNow)
  names(forecastTable)[4:6] <- c("estimate", "lower_bound", "upper_bound")
  forecastTable$date_last_update <- Sys.time()
  return(forecastTable)
}

#' @export
getForecastAllTable <- function(forecastData){
  forecastData$date_forecast <- as.Date(forecastData$time)
  forecastData$lower_bound <- forecastData$lower
  forecastData$upper_bound <- forecastData$upper
  forecastData$upper_bound <- forecastData$upper
  parties <- forecastData$party
  parties[parties == "CDU/CSU"] <- 1
  parties[parties == "SPD"] <- 2
  parties[parties == "AfD"] <- 3
  parties[parties == "GRÜNE"] <- 4
  parties[parties == "LINKE"] <- 5
  parties[parties == "FDP"] <- 6
  parties <- as.numeric(parties)
  
  forecastData$party_id <- parties
  forecastData$date_last_update <- Sys.time()
  forecastData[, c("date_forecast", "party_id", "estimate", "lower_bound", "upper_bound", "date_last_update")]
}

#' @export
eventsDE <- function(modelResults, data, predDate){
  electionForecast <- modelResults$samples$yFinal[,,1 + which(data$timeSeq == 
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
  eventData <- data.frame(date_forecast = predDate,
                          # event = c("Mehrheit für Rot-Rot-Grün",
                          #           "Mehrheit für Schwarz-Gelb",
                          #           "Mehrheit für Schwarz-Grün",
                          #           "Mehrheit für Rot-Grün",
                          #           "Mehrheit für \"Jamaika\" (Schwarz-Grün-Gelb)",
                          #           "Mehrheit für \"Ampel\" (Rot-Gelb-Grün)",
                          #           "Mehrheit für große Koalition",
                          #           "FDP kommt in den Bundestag",
                          #           "AfD kommt in den Bundestag",
                          #           "CDU/CSU bekommt mehr als 40% der Stimmen",
                          #           "SPD bekommt mehr als 30% der Stimmen",
                          #           "SPD bekommt weniger als 20% der Stimmen",
                          #           "SPD stärker als CDU/CSU",
                          #           "AfD wird drittstärkste Fraktion",
                          #           "Sechs Fraktionen im Bundestag vertreten"),
                          #           "Grüne stärker als SPD",
                          #           ),
                          event_id = c(1:15, 17:22),
                          estimate = c(
                            (((bundestag %>% select("SPD", "GRÜNE", "LINKE") %>% rowSums() > 0.5) & 
                                (bundestag %>% select("LINKE") %>% rowSums() > 0) & 
                                (bundestag %>% select("GRÜNE") %>% rowSums() > 0) & 
                                (bundestag %>% select("SPD") %>% rowSums() > 0)) %>% sum) / nrow(bundestag),
                            (((bundestag %>% select("CDU/CSU", "FDP") %>% rowSums() > 0.5) & 
                                (bundestag %>% select("FDP") %>% rowSums() > 0) & 
                                (bundestag %>% select("CDU/CSU") %>% rowSums() > 0)) %>% sum) / nrow(bundestag),
                            (((bundestag %>% select("CDU/CSU", "GRÜNE") %>% rowSums() > 0.5) & 
                                (bundestag %>% select("GRÜNE") %>% rowSums() > 0) & 
                                (bundestag %>% select("CDU/CSU") %>% rowSums() > 0)) %>% sum) / nrow(bundestag),
                            (((bundestag %>% select("SPD", "GRÜNE") %>% rowSums() > 0.5) & 
                                (bundestag %>% select("SPD") %>% rowSums() > 0) & 
                                (bundestag %>% select("GRÜNE") %>% rowSums() > 0)) %>% sum) / nrow(bundestag),
                            (((bundestag %>% select("CDU/CSU", "GRÜNE", "FDP") %>% rowSums() > 0.5) & 
                                (bundestag %>% select("FDP") %>% rowSums() > 0) & 
                                (bundestag %>% select("GRÜNE") %>% rowSums() > 0) & 
                                (bundestag %>% select("CDU/CSU") %>% rowSums() > 0)) %>% sum) / nrow(bundestag),
                            (((bundestag %>% select("SPD", "GRÜNE", "FDP") %>% rowSums() > 0.5) & 
                                (bundestag %>% select("FDP") %>% rowSums() > 0) & 
                                (bundestag %>% select("GRÜNE") %>% rowSums() > 0) & 
                                (bundestag %>% select("SPD") %>% rowSums() > 0)) %>% sum) / nrow(bundestag),
                            (((bundestag %>% select("SPD", "CDU/CSU") %>% rowSums() > 0.5) & 
                                (bundestag %>% select("SPD") %>% rowSums() > 0) & 
                                (bundestag %>% select("CDU/CSU") %>% rowSums() > 0)) %>% sum) / nrow(bundestag),
                            ((bundestag %>% select("FDP") %>% rowSums() > 0) %>% sum) / nrow(bundestag),
                            ((bundestag %>% select("AfD") %>% rowSums() > 0) %>% sum) / nrow(bundestag),
                            ((electionForecast %>% select("CDU/CSU") %>% rowSums() > 0.4) %>% sum) / nrow(electionForecast),
                            ((electionForecast %>% select("SPD") %>% rowSums() > 0.3) %>% sum) / nrow(electionForecast),
                            ((electionForecast %>% select("SPD") %>% rowSums() < 0.2) %>% sum) / nrow(electionForecast),
                            ((electionForecast %>% select("SPD") %>% rowSums() > electionForecast %>% select("CDU/CSU") %>% rowSums()) %>% sum) / nrow(electionForecast),
                            sum(apply(bundestag, 1, function(x) order(x, decreasing = TRUE)[3]) == which(colnames(bundestag) == "AfD")) / nrow(bundestag),
                            sum(apply(bundestag, 1, function(x) sum(x > 0) == 6)) / nrow(bundestag),
                            sum((electionForecast %>% select("GRÜNE") %>% rowSums()) > (electionForecast %>% select("SPD") %>% rowSums())) / nrow(electionForecast),
                            sum(apply(bundestag, 1, function(x) order(x, decreasing = TRUE)[1]) == which(colnames(bundestag) == "GRÜNE")) / nrow(bundestag),
                            ((electionForecast %>% select("GRÜNE") %>% rowSums() > 0.2) %>% sum) / nrow(electionForecast),
                            sum((electionForecast %>% select("LINKE") %>% rowSums()) > (electionForecast %>% select("FDP") %>% rowSums())) / nrow(electionForecast),
                            ((electionForecast %>% select("FDP") %>% rowSums() > 0.1) %>% sum) / nrow(electionForecast),
                            ((electionForecast %>% select("LINKE") %>% rowSums() > 0.1) %>% sum) / nrow(electionForecast)
                          )
  )
  return(eventData[1:21, ])
}

#' @export
koalitionDE <- function(koaldata, modelResults, data, predDate, expertUncertainty = FALSE){
  # koalitionenRankings <- prepareKoalitionData(koaldata)
  
  electionForecast <- modelResults$samples$yFinal[,,1 + which(data$timeSeq == 
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
  koalSim <- data.frame(coalition_id_1 = (bundestag$`CDU/CSU` > bundestag$SPD) & (bundestag$`CDU/CSU` + bundestag$SPD > 0.5) & 
                          (bundestag$`CDU/CSU` > 0) & (bundestag$`SPD` > 0),
                        coalition_id_2 = (bundestag$SPD > bundestag$GRÜNE) & (bundestag$LINKE + bundestag$SPD + bundestag$GRÜNE > 0.5) & 
                          (bundestag$`GRÜNE` > 0) & (bundestag$`SPD` > 0) & (bundestag$`LINKE` > 0),
                        coalition_id_3 = (bundestag$SPD > bundestag$GRÜNE) & (bundestag$FDP + bundestag$SPD + bundestag$GRÜNE > 0.5) & 
                          (bundestag$`GRÜNE` > 0) & (bundestag$`SPD` > 0) & (bundestag$`FDP` > 0),
                        coalition_id_4 = (bundestag$`CDU/CSU` > bundestag$GRÜNE) & (bundestag$`CDU/CSU` + bundestag$GRÜNE > 0.5) & 
                          (bundestag$`GRÜNE` > 0) & (bundestag$`CDU/CSU` > 0),
                        coalition_id_5 = (bundestag$SPD > bundestag$GRÜNE) & (bundestag$SPD + bundestag$GRÜNE > 0.5) & 
                          (bundestag$`GRÜNE` > 0) & (bundestag$`SPD` > 0),
                        coalition_id_6 = (bundestag$`CDU/CSU` < bundestag$SPD) & (bundestag$`CDU/CSU` + bundestag$SPD > 0.5) & 
                          (bundestag$`SPD` > 0) & (bundestag$`CDU/CSU` > 0),
                        coalition_id_7 = (bundestag$`CDU/CSU` > bundestag$FDP) & (bundestag$`CDU/CSU` + bundestag$FDP > 0.5) & 
                          (bundestag$`FDP` > 0) & (bundestag$`CDU/CSU` > 0),
                        coalition_id_8 = (bundestag$`CDU/CSU` > bundestag$GRÜNE) & (bundestag$`CDU/CSU` + bundestag$FDP + bundestag$GRÜNE > 0.5)& 
                          (bundestag$`FDP` > 0) & (bundestag$`CDU/CSU` > 0) & (bundestag$`GRÜNE` > 0),
                        coalition_id_9 = (bundestag$SPD < bundestag$GRÜNE) & (bundestag$SPD + bundestag$GRÜNE > 0.5) & 
                          (bundestag$`GRÜNE` > 0) & (bundestag$`SPD` > 0),
                        coalition_id_10 = (bundestag$SPD < bundestag$GRÜNE) & (bundestag$LINKE + bundestag$SPD + bundestag$GRÜNE > 0.5) & 
                          (bundestag$`GRÜNE` > 0) & (bundestag$`SPD` > 0) & (bundestag$`LINKE` > 0),
                        coalition_id_11 = (bundestag$SPD < bundestag$GRÜNE) & (bundestag$FDP + bundestag$SPD + bundestag$GRÜNE > 0.5) & 
                          (bundestag$`GRÜNE` > 0) & (bundestag$`SPD` > 0) & (bundestag$`FDP` > 0),
                        coalition_id_12 = (bundestag$`CDU/CSU` < bundestag$GRÜNE) & (bundestag$`CDU/CSU` + bundestag$GRÜNE > 0.5) & 
                          (bundestag$`CDU/CSU` > 0) & (bundestag$`GRÜNE` > 0),
                        coalition_id_13 = (bundestag$`CDU/CSU` < bundestag$GRÜNE) & (bundestag$`CDU/CSU` + bundestag$FDP + bundestag$GRÜNE > 0.5)& 
                          (bundestag$`FDP` > 0) & (bundestag$`CDU/CSU` > 0) & (bundestag$`GRÜNE` > 0),
                        coalition_id_14 = (bundestag$`CDU/CSU` > bundestag$SPD) & (bundestag$`CDU/CSU` + bundestag$FDP + bundestag$SPD > 0.5)& 
                          (bundestag$`FDP` > 0) & (bundestag$`CDU/CSU` > 0) & (bundestag$`SPD` > 0),
                        coalition_id_15 = (bundestag$`CDU/CSU` < bundestag$SPD) & (bundestag$`CDU/CSU` + bundestag$FDP + bundestag$SPD > 0.5)& 
                          (bundestag$`FDP` > 0) & (bundestag$`CDU/CSU` > 0) & (bundestag$`SPD` > 0),
                        coalition_id_16 = (bundestag$`CDU/CSU` > bundestag$GRÜNE) & (bundestag$`CDU/CSU` > bundestag$SPD) & 
                          (bundestag$`CDU/CSU` + bundestag$GRÜNE + bundestag$SPD > 0.5)& 
                          (bundestag$`GRÜNE` > 0) & (bundestag$`CDU/CSU` > 0) & (bundestag$`SPD` > 0),
                        coalition_id_17 = (bundestag$`CDU/CSU` < bundestag$GRÜNE) & (bundestag$`GRÜNE` > bundestag$SPD) & 
                          (bundestag$`CDU/CSU` + bundestag$GRÜNE + bundestag$SPD > 0.5) & 
                          (bundestag$`GRÜNE` > 0) & (bundestag$`CDU/CSU` > 0) & (bundestag$`SPD` > 0),
                        coalition_id_18 = (bundestag$`CDU/CSU` < bundestag$SPD) & (bundestag$`GRÜNE` < bundestag$SPD) & 
                          (bundestag$`CDU/CSU` + bundestag$GRÜNE + bundestag$SPD > 0.5) & 
                          (bundestag$`GRÜNE` > 0) & (bundestag$`CDU/CSU` > 0) & (bundestag$`SPD` > 0))
  
  koalSim <- koalSim[rowSums(koalSim) > 0, ]
  if (expertUncertainty) {
    koalSim <- koalSim[do.call(order, koalSim), ]
    uniqueKoalSim <- unique(koalSim)
    superiorCoalitions <- list(c(),
                               c(5),
                               c(5),
                               c(),
                               c(),
                               c(),
                               c(),
                               c(7, 4),
                               c(),
                               c(9),
                               c(9),
                               c(),
                               c(12, 7),
                               c(1, 7),
                               c(6, 7),
                               c(1, 4),
                               c(12, 9),
                               c(5, 6))
    
    uniqueCombs <- table(do.call(paste, koalSim))
    KoalitionenProp <-
      matrix(0, ncol = ncol(koalSim), nrow = length(uniqueCombs))
    colnames(KoalitionenProp) <- names(koalSim)
    
    rdirichlet <- function (n, alpha) {
      l <- length(alpha)
      x <- matrix(rgamma(length(alpha) * n, alpha), ncol = l, byrow = TRUE)
      x / rowSums(x)
    }
    
    addNonExpertCoal <- c(13, 14, 15, 16, 17, 18)
    if(length(addNonExpertCoal) > 0){
      nSim <- 10
    } else {
      nSim <- 1
    }
    results <- lapply(1:nSim, function(k){
      sapply(1:nrow(KoalitionenProp), function(i) {
        sumPostDraws <-   rowSums(sapply(1:nrow(koaldata), function(l) {
          res <- KoalitionenProp[i,]
          matching <- which(as.logical(uniqueKoalSim[i, ]))
          Knames <-
            as.integer(koaldata[l,][sort(match(matching, koaldata[l,]))])
          # add coalitions that are not covered by expert rankings
          if(any(addNonExpertCoal %in% matching)){
            addNonExpertCoalPresent <- addNonExpertCoal[which(addNonExpertCoal %in% matching)]
            Knames2 <- rep(0, length(Knames) + length(addNonExpertCoalPresent))
            nonExpertSample <- c()
            sampTotal <- 1:length(Knames2)
            for(k in 1:length(addNonExpertCoalPresent)){
              minRankK <- max(koaldata[l,][superiorCoalitions[[addNonExpertCoalPresent[k]]]]) + 1
              nSup <- sum(Knames < minRankK)
              sampPart <- sampTotal
              if(nSup > 0){
                sampPart <- sampPart[-c(1:nSup)]
              }
              if(length(sampPart) > 1){
                sampleNonExpert <- sort(sample(sampPart, 1))
              } else {
                sampleNonExpert <- sampPart
              }
              
              sampTotal <- sampTotal[sampTotal != sampleNonExpert]
              nonExpertSample <- c(nonExpertSample, sampleNonExpert)
            }
            Knames2[nonExpertSample] <- addNonExpertCoalPresent
            Knames2[-nonExpertSample] <- Knames
            Knames <- Knames2
          }
          if (length(Knames) > 1) {
            rand <-
              rowMeans(apply(rdirichlet(1000, rep(
                0.3, length(Knames)
              )), 1, sort, decreasing = T))
            res[Knames] <- rand
          } else {
            res[Knames] <- 1
          }
          res
        }))
        #apply Jeffreys dir(alpha=0.5) prior
        sumPostDraws[sumPostDraws > 0] <-
          sumPostDraws[sumPostDraws > 0] + 0.5
        #multiply by number of observed simulations
        sumPostDraws
      })
    })
    results <- Reduce("+", results) / nSim
    #apply restriction on superior coalitions (two-partner always preferred before three partner if contains all two-partner parties)
    for (i in 1:ncol(results)) {
      for (j in 1:nrow(results)) {
        if (results[j, i] > 0 &
            any(results[superiorCoalitions[[j]], i] > 0)) {
          results[j, i] <- 0
        }
      }
    }
    
    results <- sweep(results, 2,  colSums(results), "/")
    
    results <- sweep(results, 2,  uniqueCombs, "*")
    
    results <- rowSums(results)
    KoalitionenProp <- round(results / sum(results), 3)
    
    return(
      data.frame(
        date_forecast = predDate,
        coalition_id = 1:ncol(koalSim),
        estimate = KoalitionenProp
      )
    )
  } else {
    KoalitionenProp <- matrix(0, ncol = ncol(koalSim), nrow=nrow(koalSim))
    colnames(KoalitionenProp) <- names(koalSim)
    for(i in 1:nrow(koalSim)){
      Knames <- which(koalSim[i,]  == TRUE)
      Counts = sapply(1:nrow(koaldata), function(x) which.min(match(Knames, koaldata[x,]))) %>%
        factor(levels = 1:length(Knames)) %>% table + 1/2
      KoalitionenProp[i, c(Knames)] = Counts/sum(Counts)
    }
    KoalitionenProp <- KoalitionenProp %>% colMeans %>% round(3)
    
    return(data.frame(date_forecast = predDate,
                      coalition_id = 1:ncol(koalSim),
                      estimate = KoalitionenProp
    ))
  }
}

#' @export
partOfGovernmentDE <- function(koalitionProb, predDate){
  data.frame(date_forecast = predDate,
             party_id = 1:6,
             estimate = c(sum(koalitionProb[c(1, 4, 6, 7, 8, 12,14,15,16,17,18),3]),
                          sum(koalitionProb[c(1, 2, 3, 5, 6, 9, 10, 11,14,15,16,17,18),3]),
                          0,
                          sum(koalitionProb[c(2, 3, 4, 5, 8, 9, 10, 11, 12, 13, 16, 17, 18),3]),
                          sum(koalitionProb[c(2, 10),3]),
                          sum(koalitionProb[c(3, 7, 8, 11, 14, 15),3])))
}

#' @export
getFactSurvey <- function(pollData) {
  fact_survey <-
    gather(
      pollData %>% select(-c("Befragte", "Sonstige")),
      key = "parties",
      value = "estimate",
      -Datum,
      -Institut
    )
  
  parties <- fact_survey$parties
  parties[parties == "CDU/CSU"] <- 1
  parties[parties == "SPD"] <- 2
  parties[parties == "AfD"] <- 3
  parties[parties == "GRÜNE"] <- 4
  parties[parties == "LINKE"] <- 5
  parties[parties == "FDP"] <- 6
  fact_survey$party_id <- as.numeric(parties)
  fact_survey$parties <- NULL
  fact_survey <-
    fact_survey[, c("Datum", "Institut", "party_id", "estimate")]
  fact_survey <- na.omit(fact_survey)
  fact_survey$Institut[fact_survey$Institut == "Infratestdimap"] <- "Infratest dimap"
  fact_survey$Institut[fact_survey$Institut == "Kantar(Emnid)"] <- "Kantar (Emnid)"
  fact_survey$Institut[fact_survey$Institut == "Forsch’gr.Wahlen"] <- "Forschungsgruppe Wahlen"
  fact_survey
}
