#' Map party names to numeric IDs
#' Will throw an error when facing a given unknown party
#'
#' @param parties actual data of parties
#' @export
mapPartyNamesToNumbers <-
  function(parties) {
    # Check if the input contains any party we don't know yet
    allowed_parties <- parties()
    additional_party <- setdiff(unique(parties), allowed_parties)
    if (length(additional_party) > 0) {
      stop(
        "Unknown party(ies) found: ",
        paste(additional_party, collapse = ", "),
        ". Please add mapping in mapPartyNamesToNumbers"
      )
    }
    
    # Map parties onto numbers
    parties[parties == "CDU/CSU"] <- 1
    parties[parties == "SPD"] <- 2
    parties[parties == "AfD"] <- 3
    parties[parties == "GR\u00dcNE"] <- 4
    parties[parties == "LINKE"] <- 5
    parties[parties == "FDP"] <- 6
    # "Sonstige" has the 7 in the db
    parties[parties == "BSW"] <- 8
    return(as.numeric(parties))
  }


#' All parties currently known to the model
parties <-
  function() {
    c("CDU/CSU", "SPD", "GR\u00dcNE", "FDP", "LINKE", "AfD", "BSW")
  }
