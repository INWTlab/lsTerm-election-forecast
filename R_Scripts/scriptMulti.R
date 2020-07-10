predDate <- Sys.Date()

dataDE <- loadDataDE()
dataPrep <- preparePollData(dataDE$pollData, dataDE$Elections, predDate)
modelResults <- compileRunModel(dataPrep$modelData)
plotForecast <- plotElectionData(modelResults, dataPrep, predDate,
                                 start = "2016-01-01")
fact_forecast <- getForecastTable(modelResults, dataPrep, predDate)
fact_event_prob <- eventsDE(modelResults, dataPrep, predDate)
fact_coalition_prob <- koalitionDE(dataDE$Koalitionen, modelResults, dataPrep, predDate)
fact_part_of_government <- partOfGovernmentDE(fact_coalition_prob, predDate)
