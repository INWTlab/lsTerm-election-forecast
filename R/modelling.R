#' @export
compileRunModel <- function(data, iter = 800, warmup = 500,
                            control = list(max_treedepth = 8, adapt_delta = 0.8),
                            chains = 4, cores = 4, seed = 12345){
  mpModel <- stan_model(file = "stan_models/lsModelMultiT6Fast.stan")

  f <- sampling(mpModel, data = data,
                init_r = 0.1,
                pars = c("y", "alpha", "theta", "theta2", "phi", "opposition",
                         "government", "epsilon", "mu","tau", "tau2"),
                iter= iter, warmup = warmup, chains = chains, cores = cores,
                seed = seed,
                control = control)
  samples <- rstan::extract(f)
  
  return(list(model = f, samples = samples))
}