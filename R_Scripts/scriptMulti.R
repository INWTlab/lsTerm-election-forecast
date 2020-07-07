predDate <- Sys.Date()

dataDE <- loadDataDE()
dataPrep <- preparePollData(dataDE$pollData, dataDE$Elections, predDate)
modelResults <- compileRunModel(dataPrep$modelData)
plotForecast <- plotElectionData(modelResults, dataPrep, predDate,
                             start = "2016-01-01")
forecastTable <- getForecastTable(modelResults, dataPrep, predDate)
eventsProp <- eventsDE(modelResults, dataPrep, predDate)
