prepare_pred_for_coal_model <-
  function(modelResults, dataPrep, predDate) {
    parties <- mapPartyNamesToNumbers(parties = dataPrep$parties)
    forecastElection <- data.frame(date_forecast = predDate,
                                   type = "election_day",
                                   party_id = parties)
    
    election_time_seq <-
      floor(as.numeric(difftime(
        as.Date(dataPrep$nextElectionDate),
        as.Date("1970-01-04"),
        units = "weeks"
      )))
    
    election_pred <-
      modelResults$samples$yFinal[, , 1 + which(dataPrep$timeSeq == election_time_seq)] %>%
      as.data.frame() %>%
      rename_at(vars(starts_with('V')), ~ paste0("V", parties)) %>%
      mutate(sample = 1:nrow(.)) %>%
      pivot_longer(cols = names(.)[names(.) != "sample"],
                          names_to = "party_id",
                          values_to = "estimate") %>%
      mutate(
        estimate = round(logistic(estimate), 3),
        date_forecast = predDate,
        land = "Bundestag",
        year = substr(predDate, 1, 4),
        type = "election_day",
        party_id = substr(party_id, 2, 2)
      ) %>%
      mutate(
        party = case_when(
          party_id == 1 ~ "CDU/CSU",
          party_id == 2 ~ "SPD",
          party_id == 3 ~ "AfD",
          party_id == 4 ~ "GR\u00dcNE",
          party_id == 5 ~ "LINKE",
          party_id == 6 ~ "FDP",
          party_id == 8 ~ "BSW"
        )
      ) %>%
      filter(estimate >= 0.05) %>%
      group_by(sample) %>%
      mutate(seat_share = round(estimate / sum(estimate), 4)) %>%
      select(sample, land, year, party, value = estimate, seat_share)
    model_data_list <-
      lapply(split(election_pred, election_pred$sample), function(election_result) {
        model_data <- create_coalition_model_data(election_result)
        if (nrow(model_data) > 0) {
          model_data$sample <- unique(election_result$sample)
        }
        # reorder the possible coalitions by seat share
        model_data$possible_coalition_ordered <-
          sapply(model_data$possible_coalition, function(coalition) {
            election_result %>%
              filter(party %in% strsplit(coalition, split = "-")[[1]]) %>%
              arrange(-seat_share) %>%
              pull(party) %>%
              paste0(collapse = "-")
          })
        model_data
      })
    
    do.call("rbind", model_data_list)
    
  }
