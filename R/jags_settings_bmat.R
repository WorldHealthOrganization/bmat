jags_settings_bmat <- function(run_length) {
  if (run_length == "med_local") {
    n_chains <- 8
    n_iter <- 3000
    n_thin <- floor((n_iter * n_chains) / 3000)
    n_burnin <- 3000
  }
  if (run_length == "quick_local") {
    n_chains <- 8
    n_iter <- 1125
    n_thin <- floor((n_iter * n_chains) / 3000)
    n_burnin <- 1000
  }
  if (run_length == "quick") {
    n_chains <- 20
    n_iter <- 250
    n_thin <- floor((n_iter * n_chains) / 3000)
    n_burnin <- 1000
  }
  if (run_length == "long_local") {
    n_chains <- 8
    n_iter <- 75000
    n_thin <- floor((n_iter * n_chains) / 3000)
    n_burnin <- 5000
  }
  if (run_length == "long") {
    n_chains <- 20
    n_iter <- 30000
    n_thin <- floor((n_iter * n_chains) / 3000)
    n_burnin <- 5000
  }
  if (run_length == "test") {
    n_chains <- 2
    n_iter <- 5
    n_thin <- 1
    n_burnin <- 2
  }
  settings <- list(
    n_chains = n_chains,
    n_burnin = n_burnin,
    n_iter = n_iter,
    n_thin = n_thin
  )
  return(settings)
}