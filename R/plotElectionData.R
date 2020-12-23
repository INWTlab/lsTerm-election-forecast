#' @export
plotElectionData <- function(modelResults, data, predDate,
                             pollData, start = "2016-01-01", end = data$nextElectionDate){
  
  timeSeq <- as.POSIXct(data$timeSeq * 60*60*24*7,
                        origin = "1970-01-04")
  
  plotData <- do.call("rbind", lapply(1:data$modelData$NParties, function(x){
    data.frame(estimate = modelResults$samples$yFinal[,x,] %>% logistic %>% colMeans %>% round(4),
               lower = apply(modelResults$samples$yFinal[,x,] %>% logistic, 2, quantile, 0.05) %>% round(4),
               upper = apply(modelResults$samples$yFinal[,x,] %>% logistic, 2, quantile, 0.95) %>% round(4),
               time = timeSeq,
               party = data$parties[x])
  })) %>% arrange(as.character(party)) %>% filter(time >= start)
  
  plotData$party <- as.character(plotData$party)
  
  partyColors <- c("dodgerblue4", "black", "yellow", "green", "purple", "red")
  
  
  g <- ggplot(data = plotData, aes(x = time, y = estimate, group = party,
                                   colour = party)) + geom_line() + 
    geom_ribbon(aes(ymin = lower, ymax = upper,
                    fill = party),alpha = 0.3, colour = NA, show.legend = FALSE) + 
    scale_color_manual(values = partyColors) + scale_fill_manual(values=partyColors) +
    xlim(as.POSIXct(c(start, end)) + c(0, 3600 * 24)) + scale_y_continuous(limits = c(0, pmin(1, 0.01 + max(plotData$upper)))) + 
    geom_vline(xintercept = as.POSIXct(end)) + 
    geom_vline(xintercept = as.POSIXct(predDate)) + 
    annotate(geom = "text", x=as.POSIXct(predDate), y=0.02,
             label="prediction date", angle = 25, size = 2) +
    annotate(geom = "text", x=as.POSIXct(end),
             y=0, label="election date", angle = 25, size = 2)
  
  parties = c("CDU/CSU", "SPD", "GRÜNE", "LINKE", "FDP", "AfD")
  colourList = list(c(50,48,46), c(227, 0, 15), c(100, 161, 45), 
                    c(45, 10, 65), c(255, 237, 0), c(0, 158, 224))
  
  JSON <- lapply(1:length(parties), function(x){
    pData <- pollData %>% select(Institut, Datum, parties[x]) %>% na.omit
    pData <- split(pData, pData$Institut)
    ret <- list(plotData %>% filter(party == parties[x]) %>% select(-party),
                party = parties[x],
                color = colourList[[x]],
                polls = lapply(pData, function(x) list(institute = x[1,1], predicton = x[,3], time = x[,2]))
    )
    
  }) %>% toJSON()
  list(plot = g, json = JSON, plotData = plotData)
}