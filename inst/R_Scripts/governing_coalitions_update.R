# This script can be reused to update the governing_coalitions df
# I load the current file as governing_coalitions. The file lies in the
# directory inst/prepared_data/governing_coalitions.csv. After updating
# the file the new version is saved as inst/prepared_data/governing_coalitions.csv under
# the same name. The file is needed in the data preparation for the coalition model.

# The governing_coalitions df contains the year of the last two elections
# for each land and the Bundestag. It then lists the governing coalition emerging after
# the election and a binary variable indicates the most recent election.

library(lsTermElectionForecast)
library(dplyr)

# Update governing_coalitions by checking get_latest_gov_coalitions()
governing_coalitions_new <- get_latest_gov_coalitions()

# Manual adjustments: Might be different in another year ----------------------
# Adjustments for Hessen and Thüringen
# Die Anpassungen sind notwendig, weil die Regierung erst im Jahr nach der Wahl gebildet wurde.
# Die Verhandlungen haben etwas gedauert oder die Wahl lag am Ende des Wahljahres.
governing_coalitions_new <- governing_coalitions_new %>%
  mutate(
    year = ifelse(land == "Hessen", 2023, year),
    year = as.integer(year),
    year = ifelse(land == "Thüringen", 2019, year)
  )

# -------------------------------------------------------------------------------

# Read in the old governing_coalitions
governing_coalitions <-
  read.delim("inst/prepared_data/governing_coalitions.csv", sep = ",")

# Bind with the new coalitions and handle the data further
governing_coalitions <-
  bind_rows(governing_coalitions, governing_coalitions_new) %>%
  select(-latest_election) %>%
  distinct()

# keep only the latest two elections for each federal state
governing_coalitions <- governing_coalitions %>%
  group_by(land) %>%
  arrange(land, desc(year)) %>%
  mutate(year = as.numeric(year)) %>%
  top_n(2, year) %>%
  mutate(latest_election = ifelse(year == max(year), 1, 0)) %>%
  ungroup()

write.csv(governing_coalitions,
          file = "inst/prepared_data/governing_coalitions.csv", row.names = FALSE)
