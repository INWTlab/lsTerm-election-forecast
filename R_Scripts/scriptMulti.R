library("lsTermElectionForecast")
library("rstan")
library("rvest")

predDate <- as.Date("2017-09-23")
dataDE <- loadDataDE(predDate)
dataPrep <- preparePollData(dataDE$pollData, dataDE$Elections, predDate)
modelResults <- compileRunModel(dataPrep$modelData)
plotForecast <- plotElectionData(modelResults, dataPrep, predDate,
                                 dataDE$pollData, start = "2016-01-01")
plotForecast[[1]]
fact_forecast <- getForecastTable(modelResults, dataPrep, predDate)
fact_event_prob <- eventsDE(modelResults, dataPrep, predDate)
fact_coalition_prob <- koalitionDE(dataDE$Koalitionen, modelResults, dataPrep, predDate, expertUncertainty = TRUE)
fact_part_of_government <- partOfGovernmentDE(fact_coalition_prob, predDate)

