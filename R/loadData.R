#' @export
loadDataDE <- function(){
  pollData <- getPollData(predDate)
  Elections <- read.csv2("data/Elections.csv", encoding = 'UTF-8')
  return(list(pollData = pollData, Elections = Elections))
}