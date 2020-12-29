#' @export
preparePollData <- function(pollData, Elections, predDate,
                            minDate = predDate - 365 * 16) {
  pollData <- pollData %>% arrange(desc(Datum)) %>% filter(Datum > minDate)
  Elections$Datum <- as.Date(Elections$Datum)
  #Elections <- Elections %>% filter(Datum > minDate)
  
  # combine election and polling data
  partyNames <- c("CDU/CSU", "SPD", "GRÜNE", "FDP", "LINKE", "AfD")
  colnames(Elections)[1:length(partyNames)] <- partyNames
  electionsTemp <- Elections[Elections$Datum < predDate & Elections$Datum > minDate, c("Institut", "Datum", partyNames)]
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
  pollDataVec <- allData2$prop
  
  #Logit-transformation
  pollDataVec <- log(pollDataVec / (1 - pollDataVec))
  
  #create weekly sequence for state-space
  timeSeq <- seq(min(allData2[,"Datum"]), max(allData2[,"Datum"]) + 65, by = 1)
  matchedDates = match(allData2[,"Datum"], timeSeq) + as.numeric(as.character(factor(allData2$party,
                                                                                     levels = unique(allData2$party),
                                                                                     labels = 0:(length(unique(allData2$party)) - 1)))) * length(timeSeq)
  
  
  #get constants
  NParties <- length(unique(allData2$party))
  NTOTAL = length(pollDataVec)
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
  
  ElectionMatrix <- electionIndikator3
  ElectionVector <- ElectionMatrix[, NElections]
  ElectionMatrix <- ElectionMatrix[, 1:(NElections - 1)]
  
  govMatrix <- t(govMatrix)
  #govMatrix <- rbind(govMatrix, rep(0, ncol(govMatrix)))
  Zero = matrix(0, ncol = nrow(govMatrix), nrow = ncol(govMatrix))
  Zero2 = matrix(0, ncol = nrow(govMatrix), nrow = NElections - 1)
  
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
              pollData = pollDataVec,
              IMatrix = IMatrix,
              IMatrixEl = IMatrixEl,
              govMatrix = govMatrix,
              Zero = Zero,
              Zero2 = Zero2,
              electionIndikator2 = electionIndikator2,
              ElectionMatrix = ElectionMatrix,
              ElectionVector = ElectionVector,
              weight = weight)
  
  return(list(modelData = data,
              nextElectionDate = nextElectionDate,
              plotPollData = pollData, 
              timeSeq = timeSeq,
              parties = parties))
}

# prepareKoalitionData <- function(koalitionData){
#   returnData <- matrix(NA, nrow = nrow(koalitionData), ncol = ncol(koalitionData))
#   returnData[koalitionData == "Rot-Grün (SPD-Grüne)"] <- 5
#   returnData[koalitionData == "Schwarz-Rot (CDU/CSU-SPD), Große Koalition unter Führung CDU/CSU"] <- 1
#   returnData[koalitionData == "Schwarz-Gelb (CDU/CSU-FDP)"] <- 7
#   returnData[koalitionData == "Schwarz-Grün (CDU/CSU-Grüne)"] <- 4
#   returnData[koalitionData == "Schwarz-Gelb-Grün (CDU/CSU-FDP-Grüne), 'Jamaika'"] <- 8
#   returnData[koalitionData == "Rot-Rot-Grün (SPD-Linke-Grüne)"] <- 2
#   returnData[koalitionData == "Rot-Gelb-Grün (SPD-FDP-Grüne), 'Ampel'"] <- 3
#   returnData[koalitionData == "Rot-Schwarz (SPD-CDU), Große Koalition unter Führung SPD"] <- 6
#   returnData
#   
#   koalitionenRankings <- returnData
#   koalitionenRankings <- cbind(koalitionenRankings, NA, NA, NA, NA)
#   for(i in 1:nrow(koalitionenRankings)){
#     for(j in 1:(ncol(koalitionenRankings)-1)){
#       if(koalitionenRankings[i,j] == 5){
#         koalitionenRankings[i, ] <- c(koalitionenRankings[i,1:j], 9, koalitionenRankings[i,(j+1):(ncol(koalitionenRankings)-1)])
#       }
#       if(koalitionenRankings[i,j] == 2){
#         koalitionenRankings[i, ] <- c(koalitionenRankings[i,1:j], 10, koalitionenRankings[i,(j+1):(ncol(koalitionenRankings)-1)])
#       }
#       if(koalitionenRankings[i,j] == 3){
#         koalitionenRankings[i, ] <- c(koalitionenRankings[i,1:j], 11, koalitionenRankings[i,(j+1):(ncol(koalitionenRankings)-1)])
#       }
#       
#     }
#   }
#   koalitionenRankings <- as.data.frame(koalitionenRankings[,-ncol(koalitionenRankings)])
#   koalitionenRankings
# }


