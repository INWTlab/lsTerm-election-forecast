plotElectionData <- function(samples, data, predDate,
                             electionDate = "2017-09-25", start = "2016-01-01", end = "2017-09-30"){
  plotData <- lapply(1:data$NParties, function(x){
    data.frame(estimate = samples$y[,x,] %>% logistic %>% colMeans,
               lower = apply(samples$y[,x,] %>% logistic, 2, quantile, 0.025),
               upper = apply(samples$y[,x,] %>% logistic, 2, quantile, 0.975),
               time = as.POSIXct(data$timeSeq*60*60*24*7, origin = "1970-01-01"),
               party = factor(rownames(data$pollData)[x]))
               }) %>% bind_rows()
  plotPollData <- as.data.frame(logistic(t(data$pollData)))
  plotPollData <- cbind(plotPollData,
                      data.frame(time = as.POSIXct(data$timeSeq[data$matchedDates]*60*60*24*7,
                                                   origin = "1970-01-01")))

  plotPollData <- plotPollData %>% as_tibble %>% gather(key = "party",
                                                      value = "proportion", -time)
  partyColors <- c("dodgerblue4", "black", "yellow", "green", "purple", "red")
  ggplot(data = plotData, aes(x = time, y = estimate, group = party,
                            colour = party)) + geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper,
                  fill = party),alpha = 0.3, colour = NA, show.legend = FALSE) + 
  scale_color_manual(values=partyColors) +   scale_fill_manual(values=partyColors) +
  xlim(as.POSIXct(c(start, end))) + ylim(0,0.45) + 
  geom_vline(xintercept = as.POSIXct(electionDate)) + 
  geom_vline(xintercept = as.POSIXct(predDate)) + 
  annotate(geom = "text", x=as.POSIXct(predDate), y=0.02,
           label="prediction date", angle = 25, size = 2) +
  annotate(geom = "text", x=as.POSIXct(electionDate),
           y=0, label="election date", angle = 25, size = 2)  +
  geom_point(data = plotPollData, aes(x = time, y = proportion, group = party))
}