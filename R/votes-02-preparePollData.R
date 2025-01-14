#' Prepare poll data
#'
#' @param pollData poll data
#' @param elections election data
#' @param predDate prediction date
#' @param minDate minimum date
#'
#' @export
preparePollData <- function(pollData,
                            elections,
                            predDate,
                            minDate = predDate - 365 * 16) {
  pollData <-
    pollData %>%
    arrange(desc(Datum)) %>%
    filter(Datum > minDate)
  elections$Datum <- as.Date(elections$Datum)
  
  # combine election and polling data
  partyNames <- parties()
  elections <- elections %>%
    rename(`CDU/CSU` = `CDU.CSU`)
  electionsTemp <-
    elections[elections$Datum < predDate &
                elections$Datum > minDate, c("Institut", "Datum", partyNames)]
  pollsTemp <- pollData[, c("Institut", "Datum", partyNames)]
  
  electionsTemp$Election <- TRUE
  pollsTemp$Election <- FALSE
  
  allData <- rbind(pollsTemp, electionsTemp)
  allData <- allData %>%
    filter(!is.na(Datum)) %>%
    arrange(Datum)
  
  #omit polls in same week after elections (model does not work for them)
  allData[, "Datum"] <-
    floor(as.numeric(difftime(
      allData[, "Datum"], as.Date("1970-01-04"), units = "weeks"
    )))
  sameWeek <-
    which(allData[, "Datum"] %in% allData[which(allData[, "Election"] == 1), "Datum"] &
            allData[, "Election"] == 0)
  allData <- allData[-sameWeek, ]
  allData <-
    allData %>%
    group_by(Datum, Institut) %>%
    slice(n()) %>%
    ungroup() %>%
    as.data.frame()
  
  roundError <-
    apply(allData %>%
            select(-c("Datum", "Institut", "Election")), 1,
          function(x)
            c(0.005, 0.0025, 0.0005)[which(sapply(c(10, 5, 1), function(z)
              all(
                sapply(round(1000 * x, 0), function(y)
                  y %% z) < 0.00001, na.rm = TRUE
              )))[1]])
  
  roundData <- allData
  roundData[, !(names(roundData) %in% c("Datum", "Institut", "Election"))] <-
    approxRoundingError(allData %>%
                          select(-c("Datum", "Institut", "Election")), roundError)
  roundData <-
    gather(roundData,
           "party",
           "prop",
           -c("Datum", "Institut", "Election")) %>%
    na.omit
  
  allData2 <-
    gather(allData, "party", "prop", -c("Datum", "Institut", "Election")) %>%
    na.omit
  allData2 <-
    allData2 %>%
    group_by(party) %>%
    mutate(nElection = (1 + cumsum(Election))) %>%
    ungroup %>%
    as.data.frame
  allData2$Institut <-
    factor(allData2$Institut, levels = unique(allData2$Institut))
  allData2$party <-
    factor(allData2$party, levels = unique(allData2$party))
  allData2$nElection <-
    factor(allData2$nElection, levels = unique(allData2$nElection))
  
  #create pollster dummy matrix
  IMatrix <-
    model.matrix( ~ (Institut):(party) - 1, data = allData2)
  IMatrix <-
    IMatrix[, -grep("InstitutElection", colnames(IMatrix))]
  IMatrixEl <-
    model.matrix( ~ (nElection):(Institut):(party) - 1, data = allData2)
  IMatrixEl <-
    IMatrixEl[, -grep("InstitutElection", colnames(IMatrixEl))]
  
  IMatrixEl[rowSums(IMatrix) == 0, ] <- 0
  #Remove pollster variable (institute), create numeric date (weeks since 1970)
  pollDataVec <- allData2$prop
  roundError <- roundData$prop
  #Logit-transformation
  pollDataVec <- log(pollDataVec / (1 - pollDataVec))
  
  #create weekly sequence for state-space
  # prediction until election plus 4 weeks (plus 4 weeks because exact election
  # date is uncertain and we don't want the prediction to stop because of a wrong
  # prediction time frame)
  timeSeq <-
    seq(min(allData2[, "Datum"]),
        weeksBetweenOriginAndDate(max(elections$Datum)) + 4, by = 1)
  matchedDates <-
    match(allData2[, "Datum"], timeSeq) + as.numeric(as.character(factor(
      allData2$party,
      levels = unique(allData2$party),
      labels = 0:(length(unique(allData2$party)) - 1)
    ))) * length(timeSeq)
  
  #get constants
  NParties <- length(unique(allData2$party))
  NTOTAL <- length(pollDataVec)
  YTOTAL <- length(timeSeq)
  NPollsters <- length(unique(allData2$Institut)) - 1
  
  pos <-
    matrix(c(
      sapply(unique(allData2$party), function(x)
        which(allData2$party == x)[1]),
      sapply(unique(allData2$party), function(x)
        max(which(
          allData2$party == x
        )))
    ), ncol = 2)
  
  #create matrix of government parties
  govMatrix <-
    createGovMatrix(partyNames, YTOTAL, elections, timeSeq)
  
  #indicator of weeks of state-space time sequence with election week
  #indicator of weeks of state-space time sequence with election and week after election
  electionIndikator <- rep(1, YTOTAL)
  electionIndikator[match(allData[rowSums(IMatrix) == 0, "Datum"], timeSeq) + 1] <-
    0
  electionIndikator2 <- rep(1, YTOTAL)
  electionIndikator2[match(allData[rowSums(IMatrix) == 0, "Datum"], timeSeq)] <-
    0
  
  electionNumber <- rep(1, YTOTAL)
  for (i in 2:YTOTAL) {
    electionNumber[i] <- electionNumber[i - 1]
    if (electionIndikator2[i] == 0) {
      electionNumber[i] <- electionNumber[i] + 1
    }
  }
  NElections <- max(electionNumber)
  
  electionIndikator3 <- matrix(0, YTOTAL, NElections)
  electionIndikator3[1, 1] <- 0.9
  j <- 1
  for (i in 2:YTOTAL) {
    electionIndikator3[i, j] <-
      0.98 * electionIndikator3[i - 1, j] + 0.02
    if (electionIndikator2[i] == 0) {
      electionIndikator3[i, j] <- 0
      j <- j + 1
    }
    if (electionIndikator[i] == 0) {
      electionIndikator3[i, j] <- 0
    }
  }
  
  ElectionMatrix <- electionIndikator3
  ElectionVector <- ElectionMatrix[, NElections]
  ElectionMatrix <- ElectionMatrix[, 1:(NElections - 1)]
  
  govMatrix <- t(govMatrix)
  Zero <- matrix(0, ncol = nrow(govMatrix), nrow = ncol(govMatrix))
  Zero2 <- matrix(0, ncol = nrow(govMatrix), nrow = NElections - 1)
  
  weight <- exp(0.002 * (allData2$Datum - max(allData2$Datum)))
  
  nextElectionDate <-
    as.character(elections$Datum[(which(elections$Datum > predDate))[1]])
  
  plotPollData <-
    allData[, which(colnames(allData) %in% unique(allData2$party))]
  plotPollData <- cbind(plotPollData,
                        data.frame(time = as.POSIXct(allData$Datum * 60 *
                                                       60 * 24 * 7,
                                                     origin = "1970-01-04")))
  parties <- plotPollData %>%
    select(-"time") %>%
    names()
  
  plotPollData <- plotPollData %>%
    as_tibble %>%
    gather(key = "party", value = "proportion", -time)
  
  data <- list(
    NTOTAL = NTOTAL,
    YTOTAL = YTOTAL,
    NElections = NElections,
    NPollsters = NPollsters,
    NParties = NParties,
    matchedDates = matchedDates,
    pollData = pollDataVec,
    roundError = roundError,
    IMatrix = IMatrix,
    IMatrixEl = IMatrixEl,
    govMatrix = govMatrix,
    Zero = Zero,
    Zero2 = Zero2,
    electionIndikator2 = electionIndikator2,
    ElectionMatrix = ElectionMatrix,
    ElectionVector = ElectionVector,
    weight = weight
  )
  
  return(
    list(
      modelData = data,
      nextElectionDate = nextElectionDate,
      plotPollData = pollData,
      timeSeq = timeSeq,
      parties = parties
    )
  )
}

approxRoundingError <- function(prop, rounding) {
  (
    log((prop + rounding) / (1 - (prop + rounding))) -
    log((prop - rounding) / (1 - (prop - rounding)))
    ) * sqrt(1 / 12)
}
