create_coalition_model_data <- function(election_result) {
  
  # Calculate all combinations of names
  parties <- parties()
  combinations_2way <- combn(parties, 2, simplify = FALSE)
  combinations_3way <- combn(parties, 3, simplify = FALSE)
  all_combinations <- c(combinations_2way, combinations_3way)
  
  # Get all possible coalitions
  coalitions <-
    all_combinations[sapply(all_combinations, function(combination) {
      sum(election_result$seat_share[election_result$party %in% combination]) > 0.5 &
        all(combination %in% election_result$party)
    })]
  
  # Keep only 3 way coalitions that does not contain a 2 way with a majority
  possible_3way_coalitions <- coalitions[lengths(coalitions) == 3]
  valid_3way_coalitions <-
    possible_3way_coalitions[!sapply(possible_3way_coalitions,
                      function(coalition_3way) {
                        any(sapply(coalitions[lengths(coalitions) == 2],
                                   function(coalition_2way) {
                                     all(coalition_2way %in% coalition_3way)
                                   }))
                      })]
  
  coalitions <-
    append(coalitions[lengths(coalitions) == 2], valid_3way_coalitions)
  
  # Manually add some specific coalitions to data.
  # Minderheitskoalition in Thüringen
  # 3 way coalition in Sachsen-Anhalt and Schleswig-Holstein where 2 way coalition
  # already has the majority Coalition with Freie Wähler in Bayern
  if (unique(election_result$land) == "Th\u00fcringen" &
      unique(election_result$year) == 2019) {
    coalitions <-
      append(coalitions, list(c("LINKE", "SPD", "GR\u00dcNE")))
  } else if (unique(election_result$land) == "Th\u00fcringen" &
             unique(election_result$year) == 2024) {
    coalitions <- append(coalitions, list(c("CDU/CSU", "BSW", "SPD")))
  } else if (unique(election_result$land) == "Sachsen-Anhalt" &
             unique(election_result$year) == 2021) {
    coalitions <- append(coalitions, list(c("CDU/CSU", "SPD", "FDP")))
  } else if (unique(election_result$land) == "Sachsen" &
             unique(election_result$year) == 2024) {
    coalitions <- append(coalitions, list(c("CDU/CSU", "SPD")))
  } else if (unique(election_result$land) == "Schleswig-Holstein" &
             unique(election_result$year) == 2017) {
    coalitions <-
      append(coalitions, list(c("CDU/CSU", "GR\u00dcNE", "FDP")))
  } else if (unique(election_result$land) == "Bayern" &
             unique(election_result$year) == 2018) {
    coalitions <- append(coalitions, list(c("CDU/CSU", "FW")))
  }
  
  
  # Calculate seat share as difference from 50% (majority) and take sqrt of positive values
  seat_share_coalitions <-
    sapply(coalitions, function(combination) {
      sum(election_result$seat_share[election_result$party %in% combination]) - 0.5
    })
  
  if (any(seat_share_coalitions > 0)) {
  seat_share_coalitions[seat_share_coalitions > 0] <-
    sqrt(seat_share_coalitions[seat_share_coalitions > 0])
  }
  
  # For every party, get dummy about if they are part of coalition
  party_in_coalition <-
    lapply(parties, function(party)
      as.integer(sapply(coalitions, function(coalition)
        party %in% coalition)))
  
  # For every 2 way combinations, get dummy about if they are part of coalition
  combination_in_coalition <-
    lapply(combinations_2way, function(combination)
      as.integer(sapply(coalitions, function(coalition)
        all(combination %in% coalition))))
  
  # Get data frames
  coalition_df <-
    data.frame(possible_coalition = do.call(rbind, lapply(coalitions, function(coalition)
      paste(coalition, collapse = "-"))))
  party_in_coalition_df <-
    data.frame(matrix(unlist(party_in_coalition), ncol = length(party_in_coalition)))
  names(party_in_coalition_df) <- parties
  combination_in_coalition_df <-
    data.frame(matrix(
      unlist(combination_in_coalition),
      ncol = length(combination_in_coalition)
    ))
  names(combination_in_coalition_df) <-
    sapply(combinations_2way, function(coalition)
      paste(coalition, collapse = "-"))
  # Add largest_fraction_in_coalition, n_parties in coalition, seat share and bind data
  
  if (nrow(coalition_df) > 0) {
    coalition_df$land <- unique(election_result$land)
    coalition_df$year <- as.numeric(unique(election_result$year))
    coalition_df$governing_coalition_yesno <- NA
    coalition_df$largest_fraction_in_coalition <-
      apply(coalition_df, MARGIN = 1, function(coalition) {
        as.numeric(election_result$party[election_result$value ==
                                           max(election_result$value)][1] %in%
                     strsplit(coalition["possible_coalition"], "-")[[1]])
      })
  } else {
    coalition_df$largest_fraction_in_coalition <- list()
  }
  coalition_df$seat_share_coalitions <- seat_share_coalitions
  coalition_df$n_parties_in_coalition <- sapply(coalitions, length)

  output_df <-
    cbind(coalition_df,
          combination_in_coalition_df,
          party_in_coalition_df)
}

add_gov_coal_yesno_eleccol <-
  function(model_data) {
    model_data$governing_coalition_yesno <-
      apply(model_data %>% select(governing_coalition, possible_coalition),
            MARGIN = 1, function(x) {
              governing_coalition <-
                unlist(strsplit(x["governing_coalition"], ", "))
              possible_coalition <-
                unlist(strsplit(x["possible_coalition"], "-"))
              as.integer(
                all(governing_coalition %in% possible_coalition) &
                  length(governing_coalition) == length(possible_coalition)
              )
            })
    model_data <- model_data %>%
      mutate(election = paste(land, year)) %>%
      arrange(election) %>%
      mutate(election = as.numeric(as.factor(election)))
    model_data
  }
