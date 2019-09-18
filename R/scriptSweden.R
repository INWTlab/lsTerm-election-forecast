devtools::install_github("MansMeg/SwedishPolls", subdir = "RPackage")
 require('dplyr')
require('tidyr')
require('xml2')
require('rvest')
require('XML')
require('magrittr')
require('stringr')
require('zoo')
require('rstan')
predDate <- "2018-06-07"

pollData <- SwedishPolls::get_polls()
pollData <- pollData[!is.na(pollData$PublDate), ]
pollData$Institut <- pollData$house
pollData$Datum <- pollData$PublDate
pollData  <- pollData[pollData$Datum < predDate, ]

pollData[, 3:11] <- pollData[, 3:11] / 100

Elections <- read.csv2("data/ElectionSweden.csv", encoding = 'UTF-8')
Elections$Datum <- as.Date(Elections$Datum)


partyNames <- c("M", "L", "C", "KD", "S", "V", "MP", "SD", "FI")
colnames(Elections)[1:length(partyNames)] <- partyNames
electionsTemp <- Elections[Elections$Datum < predDate, c("Institut", "Datum", partyNames)]

pollsTemp <- pollData[,c("Institut", "Datum", partyNames)]
names(electionsTemp) <- names(pollsTemp)

electionsTemp$Election = TRUE
pollsTemp$Election = FALSE

allData <- rbind(pollsTemp, electionsTemp)
allData <- allData %>% filter(!is.na(Datum)) %>% arrange(Datum) %>% as.data.frame()

#omit polls in same week as elections (model does not work for them)
allData[,2] <- ceiling(as.numeric(difftime(allData[, "Datum"], as.Date("1970-01-01"), units = "weeks")))
sameWeek <- which(allData[, 2] %in% allData[which(allData[, "Election"] == 1),2] & allData[, "Election"] == 0)
allData <- allData[-sameWeek, ]


#save missing positions and replace missings 
Missing <- t((is.na(allData[,c(partyNames)]))) * 1
for(i in partyNames){
  allData[, i] <- na.locf(na.locf(allData[, i], fromLast = FALSE, na.rm = FALSE),
                          fromLast = TRUE, na.rm = FALSE)}
#create pollster dummy matrix
IMatrix <- model.matrix(~ Institut - 1, data = allData)
IMatrix <- IMatrix[, - which(colnames(IMatrix) == "InstitutElection")]

#Remove pollster variable (institute), create numeric date (weeks since 1970)
allData <- allData %>% select(-Institut)
allData <- as.matrix(allData)
pollData <- allData[, partyNames]

#Logit-transformation
pollData <- log((pollData) / (1 - (pollData)))

#create weekly sequence for state-space
timeSeq <- seq(min(allData[,"Datum"]), max(allData[,"Datum"]) + 52, by = 1)
matchedDates = match(allData[,"Datum"], timeSeq)

#get constants
NParties <- ncol(pollData)
NTOTAL = nrow(pollData)
YTOTAL = length(timeSeq)
NPollsters = ncol(IMatrix)

#create matrix of government parties
source('R/createGovMatrix.R', encoding = 'UTF-8')
govMatrix <- createGovMatrixSweden(partyNames, YTOTAL, Elections, timeSeq)

#indicator of weeks of state-space time sequence with election and week after election
electionIndikator <- rep(1, YTOTAL)
electionIndikator[match(allData[rowSums(IMatrix) == 0, "Datum"], timeSeq) + 1] <- 0
electionIndikator2 <- rep(1, YTOTAL)
electionIndikator2[match(allData[rowSums(IMatrix) == 0, "Datum"], timeSeq)] <- 0

NElections <- sum(-1 * electionIndikator2 + 1) + 1

electionIndikator3 <- matrix(0, length(electionIndikator), NElections)
electionIndikator3[1,1] <- 0
j = 1
for(i in 2:length(electionIndikator)){
  electionIndikator3[i,j] <- 0.95 * electionIndikator3[i-1,j] + 0.05
  if(electionIndikator2[i] == 0){
    electionIndikator3[i,j] <- 0
    j <- j + 1
  }
  if(electionIndikator[i] == 0){
    electionIndikator3[i,j] <- 0.05
  }
}


pollData <- t(pollData)
govMatrix <- t(govMatrix)

mpModel <- stan_model(file = "stan_models/lsModelUniFast.stan")
f <- sampling(mpModel,
              data = list(NTOTAL = NTOTAL,
                          YTOTAL = YTOTAL,
                          NElections = NElections,
                          NPollsters = NPollsters,
                          NParties = NParties,
                          matchedDates = matchedDates,
                          pollData = pollData,
                          IMatrix = IMatrix,
                          govMatrix = govMatrix,
                          Missing = Missing,
                          ElectionMatrix = electionIndikator3),
              iter= 900, warmup = 500, chains = 4, cores = 4, seed = 124567,
              control = list(max_treedepth = 16, adapt_delta = 0.85))

#Results
samples <- rstan::extract(f)
plotData <- lapply(1:NParties, function(x){
  data.frame(estimate = samples$y[,x,] %>% logistic %>% colMeans,
             lower = apply(samples$y[,x,] %>% logistic, 2, quantile, 0.025),
             upper = apply(samples$y[,x,] %>% logistic, 2, quantile, 0.975),
             time = as.POSIXct(timeSeq*60*60*24*7, origin = "1970-01-01"),
             party = factor(rownames(pollData)[x]))
}) %>% bind_rows()
plotPollData <- as.data.frame(logistic(t(pollData)))
plotPollData <- cbind(plotPollData,
                      data.frame(time = as.POSIXct(timeSeq[matchedDates]*60*60*24*7,
                                                   origin = "1970-01-01")))

plotPollData <- plotPollData %>% as_tibble %>% gather(key = "party",
                                                      value = "proportion", -time)
partyColors <- c("dodgerblue4", "black", "yellow", "green", "purple", "red", "orange", "darkblue", "violet", "grey")

ggplot(data = plotData, aes(x = time, y = estimate, group = party,
                            colour = party)) + geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper,
                  fill = party),alpha = 0.3, colour = NA, show.legend = FALSE) + 
  scale_color_manual(values=partyColors) +   scale_fill_manual(values=partyColors) +
  xlim(as.POSIXct(c("2012-01-01", "2018-09-07"))) + ylim(0,0.4) + 
  geom_vline(xintercept = as.POSIXct("2018-09-07")) + 
  geom_vline(xintercept = as.POSIXct(predDate)) + 
  annotate(geom = "text", x=as.POSIXct(predDate), y=0.02,
           label="prediction date", angle = 25, size = 2) +
  annotate(geom = "text", x=as.POSIXct("2018-09-07"),
           y=0, label="election date", angle = 25, size = 2)  +
  geom_point(data = plotPollData, aes(x = time, y = proportion, group = party), alpha = 0.3)


#election predictions

predElection <- samples$y[,,which(timeSeq == 
                                    ceiling(as.numeric(difftime(as.Date("2018-09-07"),
                                                                as.Date("1970-01-01"),
                                                                units = "weeks"))))] %>%
  logistic %>% colMeans %>% round(3)


recentPolls <- pollsTemp %>% filter(Datum <= predDate, Datum >= "2018-05-21") %>% 
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
  sqrt(mean((Results[-9,x] - Results[-9,2]) ^ 2))
}), 3)
