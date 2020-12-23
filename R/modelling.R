#' @export
compileRunModel <- function(data, iter = 600, warmup = 350,
                            control = list(max_treedepth = 13, adapt_delta = 0.8),
                            chains = 4, cores = 4, seed = 12345){
  mpModel <- rstan::stan_model(file = "stan_models/lsModelMulti.stan")
  
  f <- rstan::sampling(mpModel, data = data,
                       init_r = 0.1,
                       iter= iter, warmup = warmup, chains = chains, cores = cores,
                       seed = seed,
                       control = control)
  samples <- rstan::extract(f)
  
  return(list(model = f, samples = samples))
}