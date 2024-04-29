library(lsTermElectionForecast)
library(cmdstanr)

dev <- FALSE
predDate <- Sys.Date()


# Print session information ----

cat("System information:\n")
for (i in seq_along(sysinfo <- Sys.info()))
  cat(" ", names(sysinfo)[i], ":", sysinfo[i], "\n")
sessionInfo()


# Get and prepare data ----

dataDE <- getDataDE(predDate)
fact_survey <- getFactSurvey(dataDE$pollData)

printLatestPollPerPollster(fact_survey)

dataPrep <-
  preparePollData(dataDE$pollData,
                  dataDE$Elections,
                  predDate,
                  minDate = predDate - 365 * 20.1)


# Estimate models ----

modelResults <- compileRunModel(data = dataPrep$modelData, dev = dev)
plotForecast <- plotElectionData(modelResults, dataPrep, predDate,
                                 dataDE$pollData, start = "2020-01-01")
print(plotForecast$plot)
fact_forecast <-
  prepareForecastTable(modelResults, dataPrep, predDate)
fact_event_prob <- eventsDE(modelResults, dataPrep, predDate)
fact_coalition_prob <-
  calculate_coalition_probs(modelResults, dataPrep, predDate)
fact_part_of_government <-
  partOfGovernmentDE(fact_coalition_prob, predDate)
fact_forecast_all <- getForecastAllTable(plotForecast$plotData)
fact_coalition_prob$possible_coalition <- NULL
