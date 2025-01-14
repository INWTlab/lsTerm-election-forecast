library(dplyr)
library(rstan)
library(ggplot2)
library(tidyr)
library(cmdstanr)
devtools::load_all()
# QUERY ELECTION RESULTS --------------------------------------------------


# urls <- get_election_result_urls()
# election_results <- get_election_results_from_url(urls)
# save(election_results, file = "inst/prepared_data/election_results.RData")
load(
  system.file("prepared_data",
              "election_results.RData",
              package = "lsTermElectionForecast")
)
election_results <- split_election_results_by_year(election_results)


# PREPARE MODEL DATA FROM ELECTION RESULTS --------------------------------

model_data <-
  do.call("rbind", lapply(election_results, function(election_result) {
    create_coalition_model_data(election_result)
  }))


# ADD GOVERNING COALITION TO MODEL DATA -----------------------------------
# loads the updated governing_coalitions object, see governing_coalitions_update.R
governing_coalitions <- read.delim(
  system.file("prepared_data",
              "governing_coalitions.csv",
              package = "lsTermElectionForecast"),
  sep = ","
)

model_data <-
  governing_coalitions %>%
  left_join(model_data, by = c("land", "year"))

model_data <- add_gov_coal_yesno_eleccol(model_data)

# CREATE TRAIN AND TEST DATA ----------------------------------------------

# First test is to predict the coalition probs of the bundestagswahl 2021
# Therefore we remove all elections that happened at or after bundestagswahl
# 2021 from training

model_data_train <- model_data %>%
  filter(!(land == "Saarland" &
             year == 2022) & land != "Bayern") %>% # remove
  # saarland 2022 and bayern completely from training
  filter(!(land == "Bundestag" &
             latest_election == 1)) %>% # remove land used
  # for testing
  filter(year <= 2021) %>% # remove elections after bundestagswahl from training
  filter(!(land == "Berlin" &
             year == 2021)) %>% # remove elections after
  # bundestagswahl from training
  filter(!(land == "Mecklenburg-Vorpommern" &
             year == 2021)) # remove elections
# after bundestagswahl from training
model_data_test <- model_data %>%
  filter(land == "Bundestag" & latest_election == 1) %>%
  mutate(sample = 1) %>% # not needed here but as we changed prediction to
  # grouped prediction the modelling function expects the test df to have a "sample" column
  bind_rows(model_data %>% # currently prediction is done for groups and does not
              # work for a single group. Therefore we duplicate the pred data and
              # name it group/sample 2.
              filter(land == "Bundestag" & latest_election == 1) %>%
              mutate(sample = 2))

model <- estimate_coalition_model(model_data_train, model_data_test)

bundestag_coalition_probs <-
  model_data_test %>%
  mutate(prob = round(apply(model$y_new, 2, mean), 2)) %>%
  filter(sample == 1) %>%
  select(land,
         year,
         possible_coalition,
         governing_coalition_yesno,
         prob) %>%
  arrange(-prob)

ggplot(bundestag_coalition_probs, aes(x = reorder(possible_coalition, prob), y = prob)) +
  geom_col() +
  coord_flip() +
  labs(x = "Koalition",
       y = "Wahrscheinlichkeit",
       title = "Wahrscheinlichkeit für Koalitionen im Bundestag 2021")

# EVALUATE COALITION MODEL ------------------------------------------------

probs_for_coalitions_stan <-
  eval_coal_model_stan_multinom(model_data)
probs_for_coalitions_stan %>%
  filter(governing_coalition_yesno == 1) %>%
  summarise(mean = mean(prob, na.rm = TRUE))
probs_for_coalitions_stan %>%
  reframe(log_lik = log_likelihood(y_true = governing_coalition_yesno, y_pred = prob))

# ESTIMATE FINAL MODEL AND PREDICT COALITION FOR NEWEST POLLS -------------

# The final model contains all laender elections and the latest two bundestagswahlen
model_data_train <- model_data %>%
  filter(!(land == "Saarland" &
             year == 2022) & land != "Bayern") # remove saarland
# 2022 and bayern completly from training

# load("~/git_repos/inwt_lsTerm-election-forecast/model_results/Model_2023-08-16.RData")
# getForecastTable(modelResults, dataPrep, predDate)
# election_prediction <- prepare_pred_for_coal_model(modelResults, dataPrep, predDate)
# The model results object is 2.1GB. Thats why I already used prepare_pred_for_coal_model()
# and saved the election_prediction df only.
load(
  system.file("prepared_data",
              "election_prediction.RData",
              package = "lsTermElectionForecast")
)

model_final <-
  estimate_coalition_model(model_data_train = model_data_train,
                           model_data_test = election_prediction)

# Samples have different possible coalitions. Therefore we need to expand the
# election_prediction df to all possible coalitions before calculating the mean.
# This is because missing possible coalitions have a zero probability and would
# otherwise be ignored.
all_possible_coalitions <-
  election_prediction %>%
  group_by(sample) %>%
  expand(possible_coalition_ordered = unique(election_prediction %>%
                                               pull(possible_coalition_ordered))) %>%
  ungroup()

election_prediction_probs <-
  election_prediction %>%
  mutate(prob = round(apply(model_final$y_new, 2, mean), 3)) %>%
  select(sample, land, year, possible_coalition_ordered, prob)

election_prediction_all <-
  all_possible_coalitions %>%
  left_join(election_prediction_probs,
            by = c("sample", "possible_coalition_ordered")) %>%
  mutate(prob = ifelse(is.na(prob), 0, prob)) %>%
  mutate(land = "Bundestag", year = 2025)

coalition_probs <-
  election_prediction_all %>%
  group_by(possible_coalition_ordered) %>%
  reframe(prob = round(mean(prob), 3)) %>%
  arrange(-prob) %>%
  filter(prob > 0)

# Currently there is a distinction betweend SPD-CDU and CDU-SPD. This is
# intentional as the order of the parties is important for the chanchellor.
# However we also want values for unique coalitions e.g. the sum of SPD-CDU and CDU-SPD.
# Therefore we aggregate same coalitions with different orders.
coalition_probs$possible_coalition_unique <-
  sapply(coalition_probs$possible_coalition_ordered, function(coalition) {
    paste0(sort(strsplit(coalition, "-")[[1]]), collapse = "-")
  })

coalition_probs_unique <-
  coalition_probs %>%
  group_by(possible_coalition_unique) %>%
  reframe(prob = sum(prob)) %>%
  arrange(-prob)

ggplot(coalition_probs_unique, aes(x = reorder(possible_coalition_unique, prob), y = prob)) +
  geom_col() +
  coord_flip() +
  labs(x = "Koalition",
       y = "Wahrscheinlichkeit",
       title = "Wahrscheinlichkeit für Koalitionen im Bundestag 2025")

# PLAUSIBILTY CHECK -------------------------------------------------------
# CDU/CSU-GRÜNE 0.251
# CDU/CSU-SPD 0.158

# very similar:
# table shows cross-frequency of CDU/CSU-GRÜNE being part of possible coaltion
# and actually forming the governing coalition this table ignores whether a third
# party is involved
table(test$governing_coalition_yesno, test$`CDU/CSU-GRÜNE`)
table(test$governing_coalition_yesno, test$`CDU/CSU-SPD`)

# Many of the CDU/CSU-SPD attempts were pure CDU/CSU-SPD coalitions:
model_data_train %>%
  select(possible_coalition,
         governing_coalition_yesno,
         `CDU/CSU-SPD`,
         `CDU/CSU-GRÜNE`) %>%
  filter(`CDU/CSU-SPD` == 1 & governing_coalition_yesno == 0)

# While fails of the CDU/CSU-GRÜNE were mostly with a third party involved (e.g. AfD):
model_data_train %>%
  select(possible_coalition,
         governing_coalition_yesno,
         `CDU/CSU-SPD`,
         `CDU/CSU-GRÜNE`) %>%
  filter(`CDU/CSU-GRÜNE` == 1 & governing_coalition_yesno == 0)

# When CDU/CSU-GRÜNE was possible, they formed the governing coalition in 6 from 9 times
model_data_train %>%
  select(possible_coalition,
         governing_coalition_yesno,
         `CDU/CSU-SPD`,
         `CDU/CSU-GRÜNE`) %>%
  filter(possible_coalition == "CDU/CSU-GRÜNE") %>%
  pull(governing_coalition_yesno) %>%
  table()

# When CDU/CSU-SPD was possible, they formed the governing coalition in 6 from 24 times
model_data_train %>%
  select(possible_coalition,
         governing_coalition_yesno,
         `CDU/CSU-SPD`,
         `CDU/CSU-GRÜNE`) %>%
  filter(possible_coalition == "CDU/CSU-SPD") %>%
  pull(governing_coalition_yesno) %>%
  table()
