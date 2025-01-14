# runs the example from https://mc-stan.org/cmdstanr/articles/opencl.html
# requires installation of cmdstan and cmdstanr as in install_CmdStan.R

library(cmdstanr)

# Generate some fake data
n <- 250000
k <- 20
X <- matrix(rnorm(n * k), ncol = k)
y <- rbinom(n, size = 1, prob = plogis(3 * X[, 1] - 2 * X[, 2] + 1))
mdata <- list(k = k,
              n = n,
              y = y,
              X = X)

# Compile the model with STAN_OPENCL=TRUE
mod_cl <- cmdstan_model(system.file("stan_models",
                                    "gpu_test_logistic.stan",
                                    package = "lsTermElectionForecast"),
                        force_recompile = TRUE,
                        cpp_options = list(stan_opencl = TRUE)
                        )

# Sample from the model
fit_cl <-
  mod_cl$sample(
    data = mdata,
    chains = 4,
    parallel_chains = 4,
    opencl_ids = c(0, 0),
    refresh = 0
  )

# without GPU:
mod <- cmdstan_model(system.file("stan_models",
                                 "gpu_test_logistic.stan",
                                 package = "lsTermElectionForecast"),
                     force_recompile = TRUE)
fit_cpu <- mod$sample(data = mdata, chains = 4, parallel_chains = 4, refresh = 0)
