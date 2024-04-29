#' Based on the modeling results per party, compute probabilities of coalitions
#' @param modelResults Results of the stan model
#' @param dataPrep prepared poll data as returned by preparePollData
#' @param predDate when the predictions are created
#' @export
calculate_coalition_probs <-
  function(modelResults, dataPrep, predDate) {
    # LOAD ELECTION RESULTS ----------------------------------------------------
    
    election_results <-
      split_election_results_by_year(election_results)
    
    # PREPARE MODEL DATA FROM ELECTION RESULTS --------------------------------
    
    model_data <-
      do.call("rbind", lapply(election_results, function(election_result) {
        create_coalition_model_data(election_result)
      }))
    
    # ADD GOVERNING COALITION TO MODEL DATA -----------------------------------

    governing_coalitions <- read.delim(
      system.file("prepared_data",
                  "governing_coalitions.csv",
                  package = "lsTermElectionForecast"),
      sep = ",")
    
    model_data <-
      governing_coalitions %>%
      left_join(model_data, by = c("land", "year"))
    
    model_data <-
      add_gov_coal_yesno_eleccol(model_data)
    
    # ESTIMATE FINAL MODEL AND PREDICT COALITION FOR NEWEST POLLS -------------
    
    model_data_train <- model_data %>%
      filter(!(land == "Saarland" &
                 year == 2022) & land != "Bayern")
    
    election_prediction <-
      prepare_pred_for_coal_model(modelResults, dataPrep, predDate)
    
    model_final <-
      estimate_coalition_model(model_data_train = model_data_train,
                               model_data_test = election_prediction)
    
    # Function to generate permutations of a vector
    permute <- function(v) {
      if (length(v) <= 1) {
        return(list(v))
      } else {
        result <- list()
        for (i in seq_along(v)) {
          sub_perm <- permute(v[-i])
          for (j in seq_along(sub_perm)) {
            result[[length(result) + 1]] <- c(v[i], sub_perm[[j]])
          }
        }
        return(result)
      }
    }
    
    # Generate all possible combinations
    # Exclude coalitions with BSW for now because we lack historical data
    parties <- setdiff(parties(), "BSW")
    combinations_2way <- combn(parties, 2, simplify = FALSE)
    combinations_3way <- combn(parties, 3, simplify = FALSE)
    all_combinations <- c(combinations_2way, combinations_3way)
    
    # Permute the combinations
    permuted_combinations <- lapply(all_combinations, permute)
    flat_permutations <-
      unlist(permuted_combinations, recursive = FALSE)
    
    all_possible_coalitions_vector <-
      sapply(flat_permutations, function(coalition)
        paste0(coalition, collapse = "-"))
    
    all_possible_coalitions <-
      election_prediction %>%
      group_by(sample) %>%
      expand(possible_coalition_ordered = all_possible_coalitions_vector) %>%
      ungroup()
    
    # Calculate mean over all samples
    election_prediction_probs <-
      election_prediction %>%
      mutate(prob = round(apply(model_final$y_new, 2, mean), 3)) %>%
      select(sample, land, year, possible_coalition_ordered, prob)
    
    election_prediction_all <-
      all_possible_coalitions %>%
      left_join(election_prediction_probs,
                by = c("sample", "possible_coalition_ordered")) %>%
      mutate(prob = ifelse(is.na(prob), 0, prob)) %>%
      mutate(land = "Bundestag", year = 2023)
    
    # this is needed to get probs for chanchellors
    coalition_probs <-
      election_prediction_all %>%
      group_by(possible_coalition_ordered) %>%
      reframe(prob = round(mean(prob), 3)) %>%
      arrange(-prob)
    
    # Assign coalition_id ----------------------------------------------------
    # there 150 possible coalitions
    # id 1 - 18 are identical to old model
    coalition_probs$coalition_id[coalition_probs$possible_coalition_ordered ==
                                   "CDU/CSU-SPD"] <- 1
    coalition_probs$coalition_id[coalition_probs$possible_coalition_ordered ==
                                   "SPD-LINKE-GR\u00dcNE"] <- 2
    coalition_probs$coalition_id[coalition_probs$possible_coalition_ordered ==
                                   "SPD-FDP-GR\u00dcNE"] <- 3
    coalition_probs$coalition_id[coalition_probs$possible_coalition_ordered ==
                                   "CDU/CSU-GR\u00dcNE"] <- 4
    coalition_probs$coalition_id[coalition_probs$possible_coalition_ordered ==
                                   "SPD-GR\u00dcNE"] <- 5
    coalition_probs$coalition_id[coalition_probs$possible_coalition_ordered ==
                                   "SPD-CDU/CSU"] <- 6
    coalition_probs$coalition_id[coalition_probs$possible_coalition_ordered ==
                                   "CDU/CSU-FDP"] <- 7
    coalition_probs$coalition_id[coalition_probs$possible_coalition_ordered ==
                                   "CDU/CSU-FDP-GR\u00dcNE"] <- 8
    coalition_probs$coalition_id[coalition_probs$possible_coalition_ordered ==
                                   "GR\u00dcNE-SPD"] <- 9
    coalition_probs$coalition_id[coalition_probs$possible_coalition_ordered ==
                                   "GR\u00dcNE-SPD-LINKE"] <- 10
    coalition_probs$coalition_id[coalition_probs$possible_coalition_ordered ==
                                   "GR\u00dcNE-SPD-FDP"] <- 11
    coalition_probs$coalition_id[coalition_probs$possible_coalition_ordered ==
                                   "GR\u00dcNE-CDU/CSU"] <- 12
    coalition_probs$coalition_id[coalition_probs$possible_coalition_ordered ==
                                   "GR\u00dcNE-CDU/CSU-FDP"] <- 13
    coalition_probs$coalition_id[coalition_probs$possible_coalition_ordered ==
                                   "CDU/CSU-SPD-FDP"] <- 14
    coalition_probs$coalition_id[coalition_probs$possible_coalition_ordered ==
                                   "SPD-CDU/CSU-FDP"] <- 15
    coalition_probs$coalition_id[coalition_probs$possible_coalition_ordered ==
                                   "CDU/CSU-GR\u00dcNE-SPD"] <- 16
    coalition_probs$coalition_id[coalition_probs$possible_coalition_ordered ==
                                   "GR\u00dcNE-CDU/CSU-SPD"] <- 17
    coalition_probs$coalition_id[coalition_probs$possible_coalition_ordered ==
                                   "SPD-CDU/CSU-GR\u00dcNE"] <- 18
    
    coalition_probs_all <-
      coalition_probs %>%
      mutate(date_forecast = predDate) %>%
      select(date_forecast,
             possible_coalition = possible_coalition_ordered,
             coalition_id,
             estimate = prob) %>%
      arrange(coalition_id, possible_coalition) %>%
      mutate(across(coalition_id, ~ ifelse(is.na(.), row_number(), .)))
  }
