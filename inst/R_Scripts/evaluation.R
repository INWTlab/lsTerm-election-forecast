#__________________________________________________________________
#
# Evaluation of "wer gewinnt die wahl?" Forecasting Model 2021
#
#__________________________________________________________________
library("INWTggplot")
library("lsTermElectionForecast")
library("tidyverse")
setThemeGgplot2()

#### Volatility of Polls ####

predDate <- as.Date("2024-06-23")
dataDE <- getDataDE(predDate)

electionYear <- c(1998, 2002, 2005, 2009, 2013, 2017, 2021, 2024) # Todo: change 2024 to 2025

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
  if (x == 2024) { # todo: Change from 2024 to 2025
    maxDate <- as.Date("2024-06-23") # Todo: replace with real election date
  }
  mean(diff(apply((
    dataDE$pollData %>%
      filter(.data$Datum >= as.Date(paste0(x, "-01-01")) &
               .data$Datum <= maxDate)
  )[, 3:9], 2, range)), na.rm = TRUE) # 3:9 selects all party columns
}))

res <- as.data.frame(res)
res[, 1] <- as.factor(as.character(res[, 1]))
plot(res, ylab = "avg range between polls", type = "l")

res$V2[8] / mean(res$V2[1:7]) # Change of volatility compared to previous elections

# Create Poll Volatility Plot for Blogpost
volatility_polls_plot <- res %>%
  mutate(V2 = V2 * 100) %>%
  ggplot() +
  geom_col(aes(x = electionYear,
               y = V2),
           alpha = 0.8) +
  labs(title = "Schwankungen der Umfragewerte bei den letzten Bundestagswahlen",
       subtitle = "Durchschnittliche Spannweite der Umfragewerte im jeweiligen
       Wahljahr in Prozentpunkten") +
  ylab("Prozentpunkte") +
  theme(axis.title.x = element_blank())
volatility_polls_plot

#### Performance Analysis of our Forecasts ####

predDate <- as.Date("2024-06-23") # Todo: insert real election date
polls <- getDataDE(predDate)$pollData
polls <- polls %>%
  arrange(desc(Datum))

forecast <-
  read.csv("https://www.wer-gewinnt-die-wahl.de/forecast/forecast.csv")

partyNames <- c("CDU/CSU",
                "AfD",
                "SPD",
                "Grüne",
                "BSW",
                "FDP",
                "Linke"
                )
result <-
  t(data.frame(
    result = c(32.4, 18.7, 14.8, 12.7, 7.1, 5, 3.5) / 100, # Todo replace with election results
    row.names = partyNames
  ))

evaluation_dates <-
  seq(as.Date("2024-04-25"), as.Date("2024-06-23"), by = 1) # Todo update these dates

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
    select("CDU/CSU", "AfD", "SPD",
           "GRÜNE", "BSW", "FDP", "LINKE")
  last_five_polls <-
    polls %>%
    filter(Datum <= evaluation_dates[x] &
                       Datum > (as.Date(evaluation_dates[x]) - 14)) %>%
    group_by(Institut) %>%
    slice(1) %>%
    ungroup %>%
    select("CDU/CSU", "AfD", "SPD",
           "GRÜNE", "BSW", "FDP", "LINKE")
  last_five_polls <-
    last_five_polls[1:min(5, NROW(last_five_polls)), ] %>%
    colMeans(na.rm = TRUE)

  # Evaluation:
  
  abs_error[x, 1] <- mean(abs(result - forecast_eval))
  abs_error[x, 2] <-
    rowMeans(abs(as.matrix(result)[rep(1, nrow(last_polls), ), ] - as.matrix(last_polls)), na.rm = TRUE) %>%
    mean
  abs_error[x, 3] <- mean(abs(result - last_five_polls))
  
  sq_error[x, 1] <- mean(abs(result - forecast_eval) ^ 2) %>%
    sqrt
  sq_error[x, 2] <-
    (rowMeans(abs(
      as.matrix(result)[rep(1, nrow(last_polls), ), ] - as.matrix(last_polls)
    ) ^ 2, na.rm = TRUE) %>%
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
    ))) / as.matrix(log(result))[rep(1, nrow(last_polls), ), ]), na.rm = TRUE) %>%
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
    rep_len("INWT Prognose", length(sq_error)/3),
    rep_len("Aktuellste Umfrage", length(sq_error)/3),
    rep_len("Durchschnitt der aktuellsten 5 Umfragen", length(sq_error)/3)
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
  ylim(0, NA) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b'%y",
    limits = c(as.Date("2024-04-01"), as.Date("2024-06-23")) # Todo adapt limits to selected evaluation_dates
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
