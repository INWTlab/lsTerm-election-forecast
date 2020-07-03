#' @export
preparePollData <- function(pollData, Elections, predDate) {
  minDate <- "1998-01-01"
  pollData <- pollData %>% arrange(desc(Datum)) %>% filter(Datum > minDate)
  Elections$Datum <- as.Date(Elections$Datum)
  Elections <- Elections %>% filter(Datum > minDate)
  
  # combine election and polling data
  partyNames <- c("CDU/CSU", "SPD", "GRÜNE", "FDP", "LINKE", "AfD")
  colnames(Elections)[1:length(partyNames)] <- partyNames
  electionsTemp <- Elections[Elections$Datum < predDate, c("Institut", "Datum", partyNames)]
  pollsTemp <- pollData[,c("Institut", "Datum", partyNames)]
  names(electionsTemp) <- names(pollsTemp)
  
  electionsTemp$Election = TRUE
  pollsTemp$Election = FALSE
  
  allData <- rbind(pollsTemp, electionsTemp)
  allData <- allData %>% filter(!is.na(Datum)) %>% arrange(Datum)
  
  #omit polls in same week after elections (model does not work for them)
  allData[,2] <- floor(as.numeric(difftime(allData[, "Datum"], as.Date("1970-01-04"), units = "weeks")))
  sameWeek <- which(allData[, 2] %in% allData[which(allData[, "Election"] == 1),2] & allData[, "Election"] == 0)
  allData <- allData[-sameWeek, ]
  allData <- allData %>% group_by(Datum, Institut) %>% slice(n()) %>% ungroup() %>% as.data.frame()
  
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
  govMatrix <- createGovMatrix(partyNames, YTOTAL, Elections, timeSeq)
  
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
  
  nextElectionDate <- as.character(Elections$Datum[(which(Elections$Datum > predDate))[1]])
  
  plotPollData <- allData[, which(colnames(allData) %in% unique(allData2$party))] 
  plotPollData <- cbind(plotPollData,
                        data.frame(time = as.POSIXct(allData$Datum*60*60*24*7,
                                                     origin = "1970-01-04")))
  parties <- plotPollData %>% select(-"time") %>% names()
  
  plotPollData <- plotPollData %>% 
    as_tibble %>% gather(key = "party", value = "proportion", -time)
  
  data = list(NTOTAL = NTOTAL,
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
              weight = weight)
  
  return(list(modelData = data,
              nextElectionDate = nextElectionDate,
              plotPollData = plotPollData, 
              timeSeq = timeSeq,
              parties = parties))
}