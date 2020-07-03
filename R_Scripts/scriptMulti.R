predDate <- "2017-06-25"

dataDE <- loadDataDE()
dataPrep <- preparePollData(dataDE$pollData, dataDE$Elections, predDate)
modelResults <- compileRunModel(dataPrep$modelData)
plotForecast <- plotElectionData(modelResults, dataPrep, predDate,
                             start = "2016-01-01")
plotForecast

predElection
par(mfrow = c(2,2))
sapply(1:NParties,
       function(x) (samples$y[,x,] %>% colMeans %>% diff %>% acf(plot = FALSE,32))$acf) %>% rowMeans %>% plot(type = "h")
sapply(1:NParties,
       function(x) (samples$y[,x,] %>% colMeans %>% diff %>% pacf(plot = FALSE,32))$acf) %>% rowMeans %>% plot(type = "h")

sapply(1:NParties,
       function(x) (samples$epsilon[,,x] %>% colMeans %>% acf(plot = FALSE,32))$acf) %>% rowMeans %>% plot(type = "h")
sapply(1:NParties,
       function(x) (samples$epsilon[,,x] %>% colMeans %>% pacf(plot = FALSE,32))$acf) %>% rowMeans %>% plot(type = "h")


recentPolls <- pollsTemp %>% filter(Datum <= predDate, Datum >= (as.Date(predDate) - 14)) %>% 
  arrange(desc(Datum)) %>% group_by(Institut) %>% slice(1) %>%
  ungroup %>% arrange(desc(Datum))

recentPolls <- matrix(recentPolls[, partyNames] %>% t,
                      ncol = length(recentPolls$Institut), byrow  = F,
                      dimnames = list(NULL, recentPolls$Institut)) %>% as.data.frame()
recentPolls$Avg <- round(rowMeans(recentPolls, na.rm = TRUE),3)

Results <- data.frame(partyNames = partyNames,
                      electionResult = unlist(Elections[Elections$Year == 2018, 1:length(partyNames)]),
                      prediction = round(predElection, 3),
                      recentPolls)


#compare model and polls with actual results
round(sapply(3:ncol(Results), function(x){
  sqrt(mean((Results[,x] - Results[,"electionResult"]) ^ 2, na.rm  = TRUE))
}), 3)

