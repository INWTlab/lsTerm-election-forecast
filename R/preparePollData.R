preparePollData <- function(pollData, Elections, predDate) {
  # combine election and poll data
  partyNames <- c("CDU/CSU", "SPD", "GRÜNE", "FDP", "LINKE", "AfD")
  colnames(Elections)[1:length(partyNames)] <- partyNames
  electionsTemp <-
    Elections[Elections$Datum < predDate, c("Institut", "Datum", partyNames)]
  pollsTemp <- pollData[, c("Institut", "Datum", partyNames)]
  names(electionsTemp) <- names(pollsTemp)
  
  electionsTemp$Election = TRUE
  pollsTemp$Election = FALSE
  
  allData <- rbind(pollsTemp, electionsTemp)
  allData <- allData %>% filter(!is.na(Datum)) %>% arrange(Datum)
  
  #save missing positions and replace missings
  Missing <- t((is.na(allData[, c(partyNames)]))) * 1
  for (i in partyNames) {
    allData[, i] <-
      na.locf(
        na.locf(allData[, i], fromLast = FALSE, na.rm = FALSE),
        fromLast = TRUE,
        na.rm = FALSE
      )
  }
  
  #create pollster dummy matrix
  IMatrix <- model.matrix( ~ Institut - 1, data = allData)
  IMatrix <-
    IMatrix[,-which(colnames(IMatrix) == "InstitutElection")]
  
  #Remove pollster variable (institute), create numeric date (weeks since 1970)
  allData <- allData %>% select(-Institut)
  allData[, 1] <- ceiling(as.numeric(difftime(
    allData[, "Datum"],
    as.Date("1970-01-01"), units = "weeks"
  )))
  allData <- as.matrix(allData)
  
  pollData <- allData[, partyNames]
  
  #Logit-transformation
  pollData <- log(pollData / (1 - pollData))
  
  #create weekly sequence for state-space
  timeSeq <-
    seq(min(allData[, "Datum"]), max(allData[, "Datum"]) + 52, by = 1)
  matchedDates = match(allData[, "Datum"], timeSeq)
  
  #get constants
  NParties <- ncol(pollData)
  NTOTAL = nrow(pollData)
  YTOTAL = length(timeSeq)
  NPollsters = ncol(IMatrix)
  
  #create matrix of government parties
  source('R/createGovMatrix.R', encoding = 'UTF-8')
  govMatrix <-
    createGovMatrix(partyNames, YTOTAL, Elections, timeSeq)
  
  #indicator of weeks of state-space time sequence with election and week after election
  electionIndikator <- rep(1, YTOTAL)
  electionIndikator[match(allData[rowSums(IMatrix) == 0, "Datum"], timeSeq) + 1] <-
    0
  electionIndikator2 <- rep(1, YTOTAL)
  electionIndikator2[match(allData[rowSums(IMatrix) == 0, "Datum"], timeSeq)] <-
    0
  
  data = list(
    NTOTAL = NTOTAL,
    YTOTAL = YTOTAL,
    NPollsters = NPollsters,
    NParties = NParties,
    matchedDates = matchedDates,
    pollData = t(pollData),
    IMatrix = IMatrix,
    govMatrix = t(govMatrix),
    Missing = Missing,
    electionIndikator = electionIndikator,
    electionIndikator2 = electionIndikator2,
    timeSeq = timeSeq
  )
  return(data)
}