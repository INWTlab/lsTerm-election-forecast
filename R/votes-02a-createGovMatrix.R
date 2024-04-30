createGovMatrix <- function(partyNames, YTOTAL, Elections, timeSeq) {
  govMatrix <- matrix(
    0,
    ncol = length(partyNames),
    nrow = YTOTAL,
    dimnames = list(c(), partyNames)
  )
  
  ElectionWeeklyDates <- ceiling(as.numeric(Elections$Datum) / 7)
  
  govMatrix[, "FDP"][timeSeq <= ElectionWeeklyDates[Elections$Year == 1998]] <-
    1
  govMatrix[, "CDU/CSU"][timeSeq <= ElectionWeeklyDates[Elections$Year == 1998]] <-
    1
  
  govMatrix[, "SPD"][timeSeq > ElectionWeeklyDates[Elections$Year == 1998] &
                       timeSeq <= ElectionWeeklyDates[Elections$Year == 2002]] <-
    1
  govMatrix[, "GR\u00dcNE"][timeSeq > ElectionWeeklyDates[Elections$Year == 1998] &
                              timeSeq <= ElectionWeeklyDates[Elections$Year == 2002]] <-
    1
  
  govMatrix[, "SPD"][timeSeq > ElectionWeeklyDates[Elections$Year == 2002] &
                       timeSeq <= ElectionWeeklyDates[Elections$Year == 2005]] <-
    1
  govMatrix[, "GR\u00dcNE"][timeSeq > ElectionWeeklyDates[Elections$Year == 2002] &
                              timeSeq <= ElectionWeeklyDates[Elections$Year == 2005]] <-
    1
  
  govMatrix[, "SPD"][timeSeq > ElectionWeeklyDates[Elections$Year == 2005] &
                       timeSeq <= ElectionWeeklyDates[Elections$Year == 2009]] <-
    1
  govMatrix[, "CDU/CSU"][timeSeq > ElectionWeeklyDates[Elections$Year == 2005] &
                           timeSeq <= ElectionWeeklyDates[Elections$Year == 2009]] <-
    1
  
  govMatrix[, "FDP"][timeSeq > ElectionWeeklyDates[Elections$Year == 2009] &
                       timeSeq <= ElectionWeeklyDates[Elections$Year == 2013]] <-
    1
  govMatrix[, "CDU/CSU"][timeSeq > ElectionWeeklyDates[Elections$Year == 2009] &
                           timeSeq <= ElectionWeeklyDates[Elections$Year == 2013]] <-
    1
  
  govMatrix[, "SPD"][timeSeq > ElectionWeeklyDates[Elections$Year == 2013] &
                       timeSeq <= ElectionWeeklyDates[Elections$Year == 2017]] <-
    1
  govMatrix[, "CDU/CSU"][timeSeq > ElectionWeeklyDates[Elections$Year == 2013] &
                           timeSeq <= ElectionWeeklyDates[Elections$Year == 2017]] <-
    1
  
  govMatrix[, "SPD"][timeSeq > ElectionWeeklyDates[Elections$Year == 2017] &
                       timeSeq <= ElectionWeeklyDates[Elections$Year == 2021]] <-
    1
  govMatrix[, "CDU/CSU"][timeSeq > ElectionWeeklyDates[Elections$Year == 2017] &
                           timeSeq <= ElectionWeeklyDates[Elections$Year == 2021]] <-
    1
  
  govMatrix[, "SPD"][timeSeq > ElectionWeeklyDates[Elections$Year == 2021]] <-
    1
  govMatrix[, "GR\u00dcNE"][timeSeq > ElectionWeeklyDates[Elections$Year == 2021]] <-
    1
  govMatrix[, "FDP"][timeSeq > ElectionWeeklyDates[Elections$Year == 2021]] <-
    1
  
  return(govMatrix)
}

logistic <- function(x) {
  exp(x) / (exp(x) + 1)
}
