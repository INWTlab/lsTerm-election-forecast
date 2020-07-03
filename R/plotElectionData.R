#' @export
plotElectionData <- function(modelResults, data, predDate,
                             start = "2016-01-01"){
  
  timeSeq <- as.POSIXct(data$timeSeq * 60*60*24*7,
                        origin = "1970-01-04")
  
  plotData <- lapply(1:data$modelData$NParties, function(x){
    data.frame(estimate = modelResults$samples$y[,x,] %>% logistic %>% colMeans,
               lower = apply(modelResults$samples$y[,x,] %>% logistic, 2, quantile, 0.025),
               upper = apply(modelResults$samples$y[,x,] %>% logistic, 2, quantile, 0.975),
               time = timeSeq,
               party = data$parties[x])
  }) %>% bind_rows()
  
  partyColors <- c("dodgerblue4", "black", "yellow", "green", "purple", "red")

  
  g <- ggplot(data = plotData, aes(x = time, y = estimate, group = party,
                                   colour = party)) + geom_line() + 
    geom_ribbon(aes(ymin = lower, ymax = upper,
                    fill = party),alpha = 0.3, colour = NA, show.legend = FALSE) + 
    scale_color_manual(values = partyColors) + scale_fill_manual(values=partyColors) +
    xlim(as.POSIXct(c(start, data$nextElectionDate)) + c(0, 3600 * 24)) + scale_y_continuous(limits = c(0, 0.6)) + 
    geom_vline(xintercept = as.POSIXct(data$nextElectionDate)) + 
    geom_vline(xintercept = as.POSIXct(predDate)) + 
    annotate(geom = "text", x=as.POSIXct(predDate), y=0.02,
             label="prediction date", angle = 25, size = 2) +
    annotate(geom = "text", x=as.POSIXct(data$nextElectionDate),
             y=0, label="election date", angle = 25, size = 2)  +
    geom_point(data = data$plotPollData, aes(x = time, y = proportion, group = party), alpha = 0.3)
  g
}