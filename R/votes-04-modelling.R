#' Run stan model
#'
#' @param data data.frame
#' @param iter iterations
#' @param warmup warmup iterations
#' @param control list with additional control arguments
#' @param chains chains
#' @param parallel_chains number of cores on which chains are calculated
#' @param seed seed
#' @param dev logical Estimate a very bad, but fast model during code development
#' or in tests
#'
#' @export
# in cmdstan, total iterations are iter + warmup (difference to rstan)
compileRunModel <- function(data,
                            dev = FALSE,
                            iter = ifelse(dev, 20, 125),
                            warmup = ifelse(dev, 10, 275),
                            control = list(max_treedepth = ifelse(dev, 3, 14),
                                           adapt_delta = 0.8),
                            chains = 8,
                            parallel_chains = 8,
                            seed = 12345) {
  
  mpModel <-
    cmdstan_model(
      stan_file = system.file("stan_models",
                              "lsModelMultiT8Fast.stan",
                              package = "lsTermElectionForecast")
    )
  # cpp_options = list(stan_opencl = TRUE)) # options for GPU support
  f <-
    mpModel$sample(
      data = data,
      iter_sampling = iter,
      iter_warmup = warmup,
      parallel_chains = parallel_chains,
      chains = chains,
      seed = seed,
      max_treedepth = control[["max_treedepth"]],
      adapt_delta = control[["adapt_delta"]],
      refresh = 5,
      init = 0 # sets the initial values all to zero
    )
  # opencl_ids = c(0, 0)) # options for GPU support
  
  samples_cmdstan_yFinal <-
    f$draws(format = "draws_matrix", variables = "yFinal")
  samples_yFinal_rearranged <-
    array(NA, dim = c(chains * iter, data$NParties, data$YTOTAL))
  for (i in seq(1, data$NParties * data$YTOTAL, by = data$NParties)) {
    samples_yFinal_rearranged[, , (i + data$NParties - 1) / data$NParties] <-
      samples_cmdstan_yFinal[, i:(i + data$NParties - 1)]
  }
  
  samples <- list(yFinal = samples_yFinal_rearranged)
  return(list(model = f, samples = samples))
}
