estimate_coalition_model <-
  function(model_data_train, model_data_test) {
    model_data_train <-
      model_data_train %>% arrange(election) # important
    group_sizes <- as.vector(model_data_train$election %>% table)
    ngroups <- length(group_sizes)
    y_cat <-
      as.vector(
        tapply(model_data_train$governing_coalition_yesno, model_data_train$election, function(x) {
          which(x == 1)
        })
      )
    
    model_data_train <- model_data_train %>%
      select(
        -land,
        -year,
        -governing_coalition,
        -governing_coalition_yesno,
        -possible_coalition,
        -latest_election,
        -election
      )
    
    group_sizes_test <- as.vector(model_data_test$sample %>% table)
    ngroups_test <- length(group_sizes_test)
    
    model_data_list <- list(
      N = nrow(model_data_train),
      K1 = 3,
      # number of predictors other
      K2 = 21,
      # number of predictors parties combinations
      K3 = 6,
      # number of predictors party
      L = ngroups,
      # number of groups, each group is one election
      s = group_sizes,
      # group sizes, number of possible coalitions per election
      X = model_data_train,
      y = y_cat,
      N_new = nrow(model_data_test),
      L_new = ngroups_test,
      s_new = group_sizes_test,
      X_new = model_data_test %>% select(names(model_data_train))
    )
    
    # Estimate with cmdstanR
    mod <- cmdstan_model(
      system.file("stan_models",
                  "coalition_model.stan",
                  package = "lsTermElectionForecast")
    )
    fit <-
      mod$sample(
        data = model_data_list,
        iter_warmup = 1000,
        iter_sampling = 2000,
        chains = 3,
        parallel_chains = 3,
        seed = 202309
      )
    
    samples_cmdstan <- fit$draws(format = "draws_df")
    
    samples_rearranged <-
      rearrange_cmdstan_samples_coalition_model(samples_cmdstan, model_data_list)
    
    return(samples_rearranged)
  }

# Calculates the model leaving out one bundesland per iteration (only the latest
# election is held out). Predicts probability for the governing coalition of the
# held out bundesland.
eval_coal_model_stan_multinom <- function(model_data) {
  # Filter specific Bundesländer that should not be included in modelling
  # prepare model data within coalition_model.R
  model_data <-
    model_data %>% filter(!(land == "Saarland" &
                              year == 2022) & land != "Bayern")
  
  # test_land <- "Bundestag"
  pred_prob_for_coal_list <- list()
  for (test_land in unique(model_data$land[model_data$latest_election == 1])) {
    model_data_train <-
      model_data %>% filter(!(land == test_land &
                                latest_election == 1))
    model_data_train <-
      model_data_train %>% arrange(election) # important
    group_sizes <- as.vector(model_data_train$election %>% table)
    ngroups <- length(group_sizes)
    y_cat <-
      as.vector(
        tapply(model_data_train$governing_coalition_yesno, model_data_train$election, function(x) {
          which(x == 1)
        })
      )
    
    model_data_train <- model_data_train %>%
      select(
        -land,
        -year,
        -governing_coalition,
        -governing_coalition_yesno,
        -possible_coalition,
        -latest_election,
        -election
      )
    
    model_data_test <- model_data %>%
      filter(land == test_land &
               latest_election == 1) %>% select(-election) %>%
      mutate(sample = 1) %>%
      bind_rows(
        model_data %>%
          filter(land == test_land &
                   latest_election == 1) %>% select(-election) %>%
          mutate(sample = 2)
      )
    
    group_sizes_test <- as.vector(model_data_test$sample %>% table)
    ngroups_test <- length(group_sizes_test)
    
    model_data_list <- list(
      N = nrow(model_data_train),
      K1 = 3,
      # number of predictors other
      K2 = 15,
      # number of predictors parties combinations
      K3 = 5,
      # number of predictors party
      L = ngroups,
      # number of groups, each group is one election
      s = group_sizes,
      # group sizes, number of possible coalitions per election
      X = model_data_train,
      y = y_cat,
      N_new = nrow(model_data_test),
      L_new = ngroups_test,
      s_new = group_sizes_test,
      X_new = model_data_test %>% select(names(model_data_train))
    )
    
    # Estimate the model
    # Estimate with cmdstanR
    mod <- cmdstan_model(
      system.file("stan_models",
                  "coalition_model.stan",
                  package = "lsTermElectionForecast")
    )
    fit <-
      mod$sample(
        data = model_data_list,
        iter_warmup = 1000,
        iter_sampling = 2000,
        chains = 3,
        parallel_chains = 3,
        seed = 202309
      )
    
    samples_cmdstan <- fit$draws(format = "draws_df")
    
    samples_rearranged <-
      rearrange_cmdstan_samples_coalition_model(samples_cmdstan, model_data_list)
    
    predicted_prob_for_coalitions <-
      model_data_test %>%
      mutate(prob = round(apply(samples_rearranged$y_new, 2, mean), 2)) %>%
      filter(sample == 1) %>%
      arrange(desc(prob)) %>%
      mutate(land = test_land) %>%
      select(land,
             year,
             possible_coalition,
             governing_coalition_yesno,
             prob)
    pred_prob_for_coal_list[[test_land]] <-
      predicted_prob_for_coalitions
  }
  do.call("rbind", pred_prob_for_coal_list)
}

log_likelihood <- function(y_true, y_pred) {
  epsilon <- 1e-10
  sum(y_true * log(pmax(y_pred, epsilon)) + (1 - y_true) * log(pmax(1 - y_pred, epsilon)))
}
