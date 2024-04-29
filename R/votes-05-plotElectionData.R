#' Plot election forecast
#'
#' @param modelResults model results
#' @param data data
#' @param predDate prediction date
#' @param pollData poll data
#' @param start start date
#' @param end end date
#'
#' @export
plotElectionData <- function(modelResults,
                             data,
                             predDate,
                             pollData,
                             start = "2016-01-01",
                             end = data$nextElectionDate) {
  timeSeq <- as.POSIXct(data$timeSeq * 60 * 60 * 24 * 7,
                        origin = "1970-01-04")

  plotData <-
    do.call("rbind", lapply(1:data$modelData$NParties, function(x) {
      data.frame(
        estimate = modelResults$samples$yFinal[, x, ] %>%
          logistic %>%
          colMeans %>%
          round(4),
        lower = apply(
          modelResults$samples$yFinal[, x, ] %>%
            logistic,
          MARGIN = 2,
          function(x) quantile(x, na.rm = TRUE, probs = 0.05)
 
        ) %>%
          round(4),
        upper = apply(
          modelResults$samples$yFinal[, x, ] %>%
            logistic,
          MARGIN = 2,
          function(x) quantile(x, na.rm = TRUE, probs = 0.95)
        ) %>%
          round(4),
        time = timeSeq,
        party = data$parties[x]
      )
    })) %>%
    arrange(as.character(party)) %>%
    filter(time >= start)
  
  plotData$party <- as.character(plotData$party)
  
  plotData <- dropPredsBeforeFirstPoll(dat = plotData, pollData = pollData)
  
  partyColors <-
    c("dodgerblue4", "orange", "black", "yellow", "green", "purple", "red")
  
  g <-
    ggplot(data = plotData, aes(
      x = time,
      y = estimate,
      group = party,
      colour = party
    )) + geom_line() +
    geom_ribbon(
      aes(
        ymin = lower,
        ymax = upper,
        fill = party
      ),
      alpha = 0.3,
      colour = NA,
      show.legend = FALSE
    ) +
    scale_color_manual(values = partyColors) +
    scale_fill_manual(values = partyColors) +
    xlim(as.POSIXct(c(start, end)) + c(0, 3600 * 24)) +
    scale_y_continuous(limits = c(0, pmin(1, 0.01 + max(plotData$upper)))) +
    geom_vline(xintercept = as.POSIXct(end)) +
    geom_vline(xintercept = as.POSIXct(predDate)) +
    annotate(
      geom = "text",
      x = as.POSIXct(predDate),
      y = 0.02,
      label = "prediction date",
      angle = 25,
      size = 2
    ) +
    annotate(
      geom = "text",
      x = as.POSIXct(end),
      y = 0,
      label = "election date",
      angle = 25,
      size = 2
    )
  
  parties <- parties()
  colourList <- list(c(50, 48, 46),
                     c(227, 0, 15),
                     c(100, 161, 45),
                     c(45, 10, 65),
                     c(255, 237, 0),
                     c(0, 158, 224),
                     c(255, 153, 51))
  
  JSON <- lapply(1:length(parties), function(x) {
    pData <-
      pollData %>%
      select(Institut, Datum, parties[x]) %>%
      na.omit
    pData <- split(pData, pData$Institut)
    ret <-
      list(
        plotData %>%
          filter(party == parties[x]) %>%
          select(-party),
        party = parties[x],
        color = colourList[[x]],
        polls = lapply(pData, function(x)
          list(
            institute = x[1, 1],
            predicton = x[, 3],
            time = x[, 2]
          ))
      )
    
  }) %>%
    toJSON()
  list(plot = g,
       json = JSON,
       plotData = plotData)
}


#' Drop predictions before the first poll of a party
#' 
#' @description
#' For new parties, we don't want backwards predictions for the time before the
#' party was founded. Therefore we keep only predictions for the date of the first
#' poll with this party or later.
#' 
#' @param dat data.frame Data to be filtered, must contain columns "party" and
#' "time"
#' @param pollData data.frame Poll data, must contain columns for all parties
#' and "Datum"
#' 
dropPredsBeforeFirstPoll <- function(dat, pollData) {
  firstPoll <- pollData %>% 
    pivot_longer(cols = parties(),
                 names_to = "party") %>% 
    filter(!is.na(value)) %>%
    group_by(party) %>% 
    summarise(firstPollDate = min(Datum))
  dat %>% 
    left_join(firstPoll, by = "party") %>% 
    filter(as.Date(time) >= as.Date(firstPollDate))
}
