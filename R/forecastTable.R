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
                                 cbind(as.matrix(modelResults$samples$y[,,1 + which(data$timeSeq == 
                                                                                      floor(as.numeric(difftime(as.Date(data$nextElectionDate),
                                                                                                                as.Date("1970-01-04"),
                                                                                                                units = "weeks"))))] %>%
                                                   logistic %>% colMeans) %>% round(3), apply(modelResults$samples$y[,,1 + which(data$timeSeq == 
                                                                                                                                   floor(as.numeric(difftime(as.Date(data$nextElectionDate),
                                                                                                                                                             as.Date("1970-01-04"),
                                                                                                                                                             units = "weeks"))))] %>%
                                                                                                logistic, 2, quantile, c(0.025, 0.975)) %>% round(3) %>% t))
  
  forecastNow <- data.frame(date_forecast = predDate, type = "next_sunday", party_id = parties, cbind(as.matrix(modelResults$samples$y[,,1 + which(data$timeSeq == 
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
#' @export
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
             #           "Grüne stärker als SPD"),
             event_id = 1:16,
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
               sum(apply(bundestag, 1, function(x) order(x, decreasing = TRUE)[3]) == which(colnames(bundestag) == "AfD")) / nrow(bundestag),
               sum(apply(bundestag, 1, function(x) sum(x > 0) == 6)) / nrow(bundestag),
               sum((electionForecast %>% select("GRÜNE") %>% rowSums()) > (electionForecast %>% select("SPD") %>% rowSums())) / nrow(electionForecast)
             )
  )
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
  
  koalSim <- data.frame(coalition_id_1 = (bundestag$`CDU/CSU` > bundestag$SPD) & (bundestag$`CDU/CSU` + bundestag$SPD > 0.5),
                        coalition_id_2 = (bundestag$SPD > bundestag$GRÜNE) & (bundestag$LINKE + bundestag$SPD + bundestag$GRÜNE > 0.5),
                        coalition_id_3 = (bundestag$SPD > bundestag$GRÜNE) & (bundestag$FDP + bundestag$SPD + bundestag$GRÜNE > 0.5),
                        coalition_id_4 = (bundestag$`CDU/CSU` > bundestag$GRÜNE) & (bundestag$`CDU/CSU` + bundestag$GRÜNE > 0.5),
                        coalition_id_5 = (bundestag$SPD > bundestag$GRÜNE) & (bundestag$SPD + bundestag$GRÜNE > 0.5),
                        coalition_id_6 = (bundestag$`CDU/CSU` < bundestag$SPD) & (bundestag$`CDU/CSU` + bundestag$SPD > 0.5),
                        coalition_id_7 = (bundestag$`CDU/CSU` > bundestag$FDP) & (bundestag$`CDU/CSU` + bundestag$FDP > 0.5),
                        coalition_id_8 = (bundestag$`CDU/CSU` > bundestag$GRÜNE) & (bundestag$`CDU/CSU` + bundestag$FDP + bundestag$GRÜNE > 0.5),
                        coalition_id_9 = (bundestag$SPD < bundestag$GRÜNE) & (bundestag$SPD + bundestag$GRÜNE > 0.5),
                        coalition_id_10 = (bundestag$SPD < bundestag$GRÜNE) & (bundestag$LINKE + bundestag$SPD + bundestag$GRÜNE > 0.5),
                        coalition_id_11 = (bundestag$SPD < bundestag$GRÜNE) & (bundestag$FDP + bundestag$SPD + bundestag$GRÜNE > 0.5),
                        coalition_id_12 = (bundestag$`CDU/CSU` < bundestag$GRÜNE) & (bundestag$`CDU/CSU` + bundestag$GRÜNE > 0.5))
  koalSim <- koalSim[rowSums(koalSim) > 0, ]
  
  if (expertUncertainty) {
    koalSim <- koalSim[do.call(order, koalSim), ]
    uniqueKoalSim <- unique(koalSim)
    superiorCoalitions <- list(c(), c(5, 9), c(5, 9), c(), c(), c(), c(), c(7, 4), c(), c(5, 9), c(5, 9), c())
    
    uniqueCombs <- table(do.call(paste, koalSim))
    KoalitionenProp <-
      matrix(0, ncol = ncol(koalSim), nrow = length(uniqueCombs))
    colnames(KoalitionenProp) <- names(koalSim)
    
    rdirichlet <- function (n, alpha) {
      l <- length(alpha)
      x <- matrix(rgamma(l * n, alpha), ncol = l, byrow = TRUE)
      sm <- x %*% rep(1, l)
      x / as.vector(sm)
    }
    
    results <- sapply(1:nrow(KoalitionenProp), function(i) {
      sumPostDraws <-   rowSums(sapply(1:nrow(koaldata), function(l) {
        res <- KoalitionenProp[i,]
        matching <- which(as.logical(uniqueKoalSim[i, ]))
        Knames <-
          as.integer(koaldata[l,][sort(match(matching, koaldata[l,]))])
        if (length(Knames) > 1) {
          rand <-
            rowMeans(apply(rdirichlet(1000, rep(
              0.5, length(Knames)
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
             estimate = c(sum(koalitionProb[c(1, 4, 6, 7, 8, 12),3]),
                          sum(koalitionProb[c(1, 2, 3, 5, 6, 9, 10, 11),3]),
                          0,
                          sum(koalitionProb[c(2, 3, 4, 5, 8, 9, 10, 11, 12),3]),
                          sum(koalitionProb[c(2, 10),3]),
                          sum(koalitionProb[c(3, 7, 8, 11),3])))
}
