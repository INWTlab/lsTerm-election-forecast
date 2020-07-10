#' @export
loadDataDE <- function(){
  pollData <- getPollData(predDate)
  Elections <- read.csv2("data/Elections.csv", encoding = 'UTF-8')
  Koalitionen <- read.csv2("data/Koalitionen.csv", encoding = 'UTF-8')
  return(list(pollData = pollData, Elections = Elections, Koalitionen = Koalitionen))
}