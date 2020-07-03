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
#require('cmdstanr')
source('R/getPollData.R', encoding = 'UTF-8')

predDate <- "2018-03-07"
minDate <- "1998-01-01"
  
pollData <- SwedishPolls::get_polls()
pollData <- pollData[!is.na(pollData$PublDate), ]
pollData$Institut <- pollData$house
pollData$Datum <- pollData$PublDate
pollData  <- pollData[pollData$Datum < predDate, ]
pollData[, 3:11] <- pollData[, 3:11] / 100

pollData %>% filter(Datum > minDate)

Elections <- read.csv2("data/ElectionSweden.csv", encoding = 'UTF-8')
Elections$Datum <- as.Date(Elections$Datum)
Elections <- Elections %>% filter(Datum > minDate)

# combine election and polling data
partyNames <- c("M", "L", "C", "KD", "S", "V", "MP", "SD", "FI")
colnames(Elections)[1:length(partyNames)] <- partyNames
electionsTemp <- Elections[Elections$Datum < predDate, c("Institut", "Datum", partyNames)]

pollsTemp <- pollData[,c("Institut", "Datum", partyNames)]
names(electionsTemp) <- names(pollsTemp)

electionsTemp$Election = TRUE
pollsTemp$Election = FALSE

allData <- rbind(pollsTemp, electionsTemp)
allData <- allData %>% filter(!is.na(Datum)) %>% arrange(Datum) %>% as.data.frame()

#omit polls in same week after elections (model does not work for them)
allData[,2] <- floor(as.numeric(difftime(allData[, "Datum"], as.Date("1970-01-04"), units = "weeks")))
sameWeek <- which(allData[, 2] %in% allData[which(allData[, "Election"] == 1),2] & allData[, "Election"] == 0)
allData <- allData[-sameWeek, ]
allData <- allData %>% group_by_(~ Datum, ~ Institut) %>% slice(n()) %>% ungroup() %>% as.data.frame()

allData2 <- gather(allData,"party", "prop", - c("Datum", "Institut", "Election")) %>% na.omit
allData2 <- allData2 %>% group_by(party) %>% mutate(nElection = (1 + cumsum(Election))) %>% ungroup %>% as.data.frame
allData2$Institut <- factor(allData2$Institut, levels = unique(allData2$Institut))
allData2$party <- factor(allData2$party, levels = unique(allData2$party))
allData2$nElection <- factor(allData2$nElection, levels = unique(allData2$nElection))

#create pollster dummy matrix
IMatrix <- model.matrix(~ (Institut):(party) - 1, data = allData2)
IMatrix <- IMatrix[, - grep("InstitutElection", colnames(IMatrix))]
IMatrixEl <- model.matrix(~ (nElection):(Institut):(party) - 1, data = allData2)
IMatrixEl <- IMatrixEl[, - grep("InstitutElection", colnames(IMatrixEl))]

IMatrixEl[rowSums(IMatrix) == 0, ] <- 0
#Remove pollster variable (institute), create numeric date (weeks since 1970)
#allData2 <- allData2 %>% select(-Institut)
pollData <- allData2$prop

#Logit-transformation
pollData <- log(pollData / (1 - pollData))

#create weekly sequence for state-space
timeSeq <- seq(min(allData2[,"Datum"]), max(allData2[,"Datum"]) + 65, by = 1)
matchedDates = match(allData2[,"Datum"], timeSeq) + as.numeric(as.character(factor(allData2$party,
                                                                                   levels = unique(allData2$party),
                                                                                   labels = 0:(length(unique(allData2$party)) - 1)))) * length(timeSeq)


#get constants
NParties <- length(unique(allData2$party))
NTOTAL = length(pollData)
YTOTAL = length(timeSeq)
NPollsters = length(unique(allData2$Institut)) - 1

pos <- matrix(c(sapply(unique(allData2$party), function(x) which(allData2$party == x)[1]),
                sapply(unique(allData2$party), function(x) max(which(allData2$party == x)))), ncol = 2)

#create matrix of government parties
source('R/createGovMatrix.R', encoding = 'UTF-8')
govMatrix <- createGovMatrixSweden(partyNames, YTOTAL, Elections, timeSeq)

#indicator of weeks of state-space time sequence with election week
#indicator of weeks of state-space time sequence with election and week after election
electionIndikator <- rep(1, YTOTAL)
electionIndikator[match(allData[rowSums(IMatrix) == 0, "Datum"], timeSeq) + 1] <- 0
electionIndikator2 <- rep(1, YTOTAL)
electionIndikator2[match(allData[rowSums(IMatrix) == 0, "Datum"], timeSeq)] <- 0

electionNumber <- rep(1, YTOTAL)
for(i in 2:YTOTAL){
  electionNumber[i] <- electionNumber[i-1]
  if(electionIndikator2[i] == 0){
    electionNumber[i] <- electionNumber[i] + 1  
  }
}
NElections <- max(electionNumber)

electionIndikator3 <- matrix(0, YTOTAL, NElections)
electionIndikator3[1,1] <- 0.9
j = 1
for(i in 2:YTOTAL){
  electionIndikator3[i,j] <- 0.98 * electionIndikator3[i-1,j] + 0.02
  if(electionIndikator2[i] == 0){
    electionIndikator3[i,j] <- 0
    j <- j + 1
  }
  if(electionIndikator[i] == 0){
    electionIndikator3[i,j] <- 0
  }
}

govMatrix <- t(govMatrix)
Zero = matrix(0, ncol = nrow(govMatrix), nrow = ncol(govMatrix))
Zero2 = matrix(0, ncol = nrow(govMatrix), nrow = NElections)

weight <- exp(0.002 * (allData2$Datum - max(allData2$Datum)))


mpModel <- stan_model(file = "stan_models/lsModelMultiT6Fast.stan")
#mpModel <- cmdstan_model(stan_file = "stan_models/lsModelMultiT4Fast.stan")
set.seed(12345)

f <- sampling(mpModel, data = list(NTOTAL = NTOTAL,
                                   YTOTAL = YTOTAL,
                                   NElections = NElections,
                                   NPollsters = NPollsters,
                                   NParties = NParties,
                                   matchedDates = matchedDates,
                                   pollData = pollData,
                                   IMatrix = IMatrix,
                                   IMatrixEl = IMatrixEl,
                                   Zero = Zero,
                                   Zero2 = Zero2,
                                   govMatrix = govMatrix,
                                   electionIndikator2 = electionIndikator2,
                                   ElectionMatrix = electionIndikator3,
                                   weight = weight),
              init_r = 0.1,
              pars = c("y", "alpha", "theta", "theta2", "phi", "opposition",
                       "government", "epsilon", "mu","tau", "tau2"),
              iter= 500, warmup = 300, chains = 4, cores = 4, seed = 124567,
              control = list(max_treedepth = 14, adapt_delta = 0.8))

samples <- rstan::extract(f)


plotData <- lapply(1:NParties, function(x){
  data.frame(estimate = samples$y[,x,] %>% logistic %>% colMeans,
             lower = apply(samples$y[,x,] %>% logistic, 2, quantile, 0.025),
             upper = apply(samples$y[,x,] %>% logistic, 2, quantile, 0.975),
             time = as.POSIXct(timeSeq*60*60*24*7, origin = "1970-01-04"),
             party = factor(unique(allData2$party)[x]))
}) %>% bind_rows()

plotPollData <- allData[, which(colnames(allData) %in% unique(allData2$party))] 
plotPollData <- cbind(plotPollData,
                      data.frame(time = as.POSIXct(allData$Datum*60*60*24*7,
                                                   origin = "1970-01-04")))

plotPollData <- plotPollData %>% as_tibble %>% gather(key = "party",
                                                      value = "proportion", -time)

partyColors <- c("dodgerblue4", "black", "yellow", "green", "purple", "red", "orange", "darkblue", "violet", "grey")


nexElectionDate <- as.character(Elections$Datum[(which(Elections$Datum > predDate))[1]])
  

g <- ggplot(data = plotData, aes(x = time, y = estimate, group = party,
                                 colour = party)) + geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper,
                  fill = party),alpha = 0.3, colour = NA, show.legend = FALSE) + 
  scale_color_manual(values=partyColors) +   scale_fill_manual(values=partyColors) +
  xlim(as.POSIXct(c("1998-01-01", nexElectionDate))) + ylim(0,0.5) + 
  geom_vline(xintercept = as.POSIXct(nexElectionDate)) + 
  geom_vline(xintercept = as.POSIXct(predDate)) + 
  annotate(geom = "text", x=as.POSIXct(predDate), y=0.02,
           label="prediction date", angle = 25, size = 2) +
  annotate(geom = "text", x=as.POSIXct(nexElectionDate),
           y=0, label="election date", angle = 25, size = 2)  +
  geom_point(data = plotPollData, aes(x = time, y = proportion, group = party), alpha = 0.3)


#election predictions

predElection <- samples$y[,,which(timeSeq == 
                                    ceiling(as.numeric(difftime(as.Date(nexElectionDate),
                                                                as.Date("1970-01-04"),
                                                                units = "weeks"))))] %>%
  logistic %>% colMeans %>% round(3)


recentPolls <- pollsTemp %>% filter(Datum <= predDate, Datum >= (as.Date(predDate) - 30)) %>% 
  arrange(desc(Datum)) %>% group_by(Institut) %>% slice(1) %>%
  ungroup %>% arrange(desc(Datum))

recentPolls <- matrix(recentPolls[, partyNames] %>% t,
                      ncol = length(recentPolls$Institut), byrow  = F,
                      dimnames = list(NULL, recentPolls$Institut)) %>% as.data.frame()
recentPolls$Avg <- round(rowMeans(recentPolls, na.rm = TRUE),3)

Results <- data.frame(partyNames = partyNames,
                      electionResult = unlist(Elections[Elections$Datum == nexElectionDate, 1:length(partyNames)]),
                      prediction = round(predElection, 3),
                      recentPolls)

#compare model and polls with actual results
round(sapply(3:ncol(Results), function(x){
  sqrt(mean((Results[-9,x] - Results[-9,"electionResult"]) ^ 2, na.rm  = TRUE))
}), 4)
