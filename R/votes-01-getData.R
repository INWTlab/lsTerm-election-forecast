#' Load data
#'
#' @param predDate prediction date
#'
#' @export
getDataDE <- function(predDate) {
  pollData <- getPollData(predDate)
  Elections <-
    read.csv2(
      system.file("prepared_data",
                  "Elections.csv",
                  package = "lsTermElectionForecast"),
      encoding = "UTF-8"
    )
  return(list(pollData = pollData, Elections = Elections))
}
