#__________________________________________________________________
#
# Evaluation of "wer gewinnt die wahl?" Forecasting Model 2021
#
#__________________________________________________________________

library("hrbrthemes")
library("lsTermElectionForecast")
library("tidyverse")

darkblue <- "#5071b0"
lightblue <- "#b3d0ec"
red <- "#bc423a"

#### Volatility of Polls ####

predDate <- as.Date("2021-09-25")
dataDE <- getDataDE(predDate)

electionYear <- c(1998, 2002, 2005, 2009, 2013, 2017, 2021)

res <- cbind(electionYear, sapply(electionYear, function(x) {
  if (x == 1998) {
    maxDate <- as.Date("1998-09-26")
  }
  if (x == 2002) {
    maxDate <- as.Date("2002-09-21")
  }
  if (x == 2005) {
    maxDate <- as.Date("2005-09-17")
  }
  if (x == 2009) {
    maxDate <- as.Date("2009-09-26")
  }
  if (x == 2013) {
    maxDate <- as.Date("2013-09-21")
  }
  if (x == 2017) {
    maxDate <- as.Date("2017-09-23")
  }
  if (x == 2021) {
    maxDate <- as.Date("2021-09-25")
  }
  mean(diff(apply((
    dataDE$pollData %>%
      filter(.data$Datum >= as.Date(paste0(x, "-01-01")) &
               .data$Datum <= maxDate)
  )[, 3:7], 2, range)))
}))

res <- as.data.frame(res)
res[, 1] <- as.factor(as.character(res[, 1]))
plot(res, ylab = "avg range between polls", type = "l")

res$V2[7] / mean(res$V2[1:6]) # volatility 77% higher than in previous elections

# Create Poll Volatility Plot for Blogpost
volatility_polls_plot <- res %>%
  mutate(V2 = V2 * 100) %>%
  ggplot() +
  geom_col(aes(x = electionYear,
               y = V2),
           fill = darkblue,
           alpha = 0.8) +
  theme_ipsum_ps() +
  labs(title = "Schwankungen der Umfragewerte bei den letzten Bundestagswahlen",
       subtitle = "Durchschnittliche Spannweite der Umfragewerte im jeweiligen
       Wahljahr in Prozentpunkten") +
  ylab("Prozentpunkte") +
  theme(axis.title.x = element_blank())
volatility_polls_plot

#### Performance Analysis of our Forecasts ####

predDate <- as.Date("2021-09-23")
polls <- getDataDE(predDate)$pollData
polls <- polls %>%
  arrange(desc(Datum))

forecast <-
  read.csv("https://www.wer-gewinnt-die-wahl.de/forecast/forecast.csv")

partyNames <- c("CDU/CSU",
                "SPD",
                "Grüne",
                "FDP",
                "Linke",
                "AfD")
result <-
  t(data.frame(
    result = c(24.071, 25.743, 14.753, 11.454, 4.890, 10.345) / 100,
    row.names = partyNames
  ))

evaluation_dates <-
  seq(as.Date("2020-12-12"), as.Date("2021-09-23"), by = 1)

abs_error <- matrix(nrow = length(evaluation_dates), ncol = 3)
sq_error <- matrix(nrow = length(evaluation_dates), ncol = 3)
rel_error <- matrix(nrow = length(evaluation_dates), ncol = 3)

for (x in 1:length(evaluation_dates)) {
  forecast_eval <- forecast %>%
    filter(type == "election_day",
           date_forecast == max(forecast$date_forecast[forecast$date_forecast
                                                       <= (evaluation_dates[x])]))
  
  forecast_eval <-
    forecast_eval[match(partyNames, forecast_eval$descr_german), "estimate"]
  
  last_polls_date <-
    max(polls$Datum[polls$Datum <= evaluation_dates[x]])
  last_polls <-
    polls %>%
    filter(Datum == last_polls_date) %>%
    select("CDU/CSU", "SPD",
           "GRÜNE", "FDP", "LINKE", "AfD")
  last_five_polls <-
    polls %>%
    filter(Datum <= evaluation_dates[x] &
                       Datum > (as.Date(evaluation_dates[x]) - 14)) %>%
    group_by(Institut) %>%
    slice(1) %>%
    ungroup %>%
    select("CDU/CSU", "SPD",
           "GRÜNE", "FDP", "LINKE", "AfD")
  last_five_polls <-
    last_five_polls[1:min(5, NROW(last_five_polls)), ] %>%
    colMeans
  
  # Evaluation:
  
  abs_error[x, 1] <- mean(abs(result - forecast_eval))
  abs_error[x, 2] <-
    rowMeans(abs(as.matrix(result)[rep(1, nrow(last_polls), ), ] - as.matrix(last_polls))) %>%
    mean
  abs_error[x, 3] <- mean(abs(result - last_five_polls))
  
  sq_error[x, 1] <- mean(abs(result - forecast_eval) ^ 2) %>%
    sqrt
  sq_error[x, 2] <-
    (rowMeans(abs(
      as.matrix(result)[rep(1, nrow(last_polls), ), ] - as.matrix(last_polls)
    ) ^ 2) %>%
      sqrt %>%
      mean)
  sq_error[x, 3] <-
    (mean(abs(result - last_five_polls) ^ 2) %>%
       sqrt)
  
  rel_error[x, 1] <-
    mean(abs((log(result) - log(forecast_eval)) / log(result)))
  rel_error[x, 2] <-
    rowMeans(abs((as.matrix(log(
      result
    ))[rep(1, nrow(last_polls), ), ] - as.matrix(log(
      last_polls
    ))) / as.matrix(log(result))[rep(1, nrow(last_polls), ), ])) %>%
    mean
  rel_error[x, 3] <-
    mean(abs((log(result) - log(last_five_polls)) / log(result)))
  
}

# Compare INWT, Latest Poll, Last 5 Polls Average
summary(abs_error)
summary(sq_error)

plot(abs_error[, 1] ~ evaluation_dates,
     type = "l",
     ylim = c(0, 0.1))
lines(abs_error[, 2] ~ evaluation_dates, col = "red")
lines(abs_error[, 3] ~ evaluation_dates, col = "blue")

plot(sq_error[, 1] ~ evaluation_dates,
     type = "l",
     ylim = c(0, 0.1))
lines(sq_error[, 2] ~ evaluation_dates, col = "red")
lines(sq_error[, 3] ~ evaluation_dates, col = "blue")

apply(abs_error, 2, max)
apply(sq_error, 2, max)

# Create Squared Error Plot for Blogpost
sq_error_data <- data.frame(
  sq_error = c(sq_error[, 1],
               sq_error[, 2],
               sq_error[, 3]),
  sq_error_comp = c(
    sq_error[, 1] - sq_error[, 1],
    sq_error[, 1] - sq_error[, 2],
    sq_error[, 1] - sq_error[, 3]
  ),
  origin = factor(c(
    rep_len("INWT Prognose", len),
    rep_len("Aktuellste Umfrage", len),
    rep_len("Durchschnitt der aktuellsten 5 Umfragen", len)
  )),
  date = rep(evaluation_dates, times = 3)
)

sq_error_plot <- sq_error_data %>%
  filter(date > "2021-01-01") %>%
  ggplot() +
  geom_line(aes(
    x = date,
    y = sq_error,
    group = origin,
    color = origin
  )) +
  theme_ipsum_ps() +
  ylim(0, NA) +
  scale_colour_manual(values = c(lightblue, darkblue, red)) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b'%y",
    limits = c(as.Date("2021-01-01"), as.Date("2021-09-24"))
  ) +
  labs(title = "Abweichung der Prognosen vom tatsächlichen Wahlergebnis",
       subtitle = "Gemessen als quadratischer Fehler („Squared Error“)") +
  ylab("Squared Error") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.title.x = element_blank()
  )
sq_error_plot
