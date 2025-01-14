# This script creates a plot of the probabilities of the top n coalitions
# since a certain date. It also shows the probabilities of the coalitions and
# the predicted vote shares of the parties on a certain day for comparison.
# Further, it shows the development of the polls over time.

library(dplyr)
library(dbtools)
library(rjson)
library(ggplot2)

# Plot -----------------------------------------------------------------------
start_date <- "2023-12-01" # when the plot starts
n_top_coalitions <- 5 # how many coalitions are shown in the plot

creds <- fromJSON(file = "creds.json")

cred <- Credentials(
  drv = MySQL,
  username = creds$username,
  password = creds$password,
  dbname = "election_forecast",
  host = creds$host,
  port = 3306,
  client.flag = CLIENT_SSL
)

dim_party <-
  sendQuery(db = cred, query = "select * from dim_party;") %>%
  rename(party_id = id)
dim_coalition <-
  sendQuery(db = cred, query = "select * from dim_coalition;") %>%
  rename(coalition_id = id)

fact_coalition_prob <-
  sendQuery(db = cred, query = "select * from fact_coalition_prob;") %>%
  left_join(dim_coalition, by = "coalition_id") %>%
  filter(date_forecast > start_date)

most_probable_coalitions <-
  fact_coalition_prob %>% arrange(desc(estimate)) %>% select(coalition_id) %>%
  unique() %>% slice(1:n_top_coalitions)

fact_coalition_prob_top <-
  fact_coalition_prob %>% filter(coalition_id %in% most_probable_coalitions$coalition_id)

fact_coalition_prob_top %>% 
  mutate(date_forecast = as.Date(date_forecast)) %>% 
  ggplot(aes(x = date_forecast, y = estimate, colour = descr_german, group = descr_german)) +
  geom_point(aes(color = descr_german), shape = 1) +
  geom_line(size = 0.6) +
  geom_vline(xintercept = as.Date("2024-03-12")) +
  labs(x = "Date", y = "Probability") +
  theme_linedraw()

# Vergleich von Koalitionswk. und Stimmanteilprognosen -------------------------
date_comparison <- "2024-04-01"
fact_coalition_prob_top %>% filter(date_forecast == date_comparison) %>%
  select(coalition_id, estimate) %>%
  left_join(dim_coalition, by = "coalition_id") %>%
  arrange(desc(estimate)) %>%
  select(descr_german, estimate) %>%
  knitr::kable() # print a table with the results

fact_forecast <-
  sendQuery(db = cred, query = "select * from fact_forecast;") %>%
  filter(date_forecast == date_comparison) %>%
  select(party_id, estimate, type)
fact_forecast %>%
  left_join(dim_party, by = "party_id") %>%
  filter(type == "election_day") %>%
  arrange(desc(estimate)) %>%
  select(descr_german, estimate) %>%
  knitr::kable() # print a table with the results

# Entwicklung der Umfragen ---------------------------------------------------
institute <-
  "INSA" # could be Allensbach, Forsa, INSA, Yougov, Infratest dimap
fact_survey <-
  sendQuery(db = cred, query = "select * from fact_survey;") %>%
  left_join(dim_party, by = "party_id") %>%
  filter(as.Date(Datum) > as.Date(start_date),
         Institut == institute) %>%
  mutate(Datum = as.Date(Datum))

fact_survey %>% select(Datum, estimate, descr_german) %>%
  arrange(Datum) %>% 
  ggplot(aes(x = Datum, y = estimate, colour = descr_german, group = descr_german)) +
  geom_point(aes(color = descr_german), shape = 1) +
  geom_line(size = 0.6) +
  geom_vline(xintercept = as.Date("2024-03-12")) +
  labs(x = "Date", y = "Probability") +
  theme_linedraw()
