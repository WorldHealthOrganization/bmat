jags_list_settings <- function(run_setting) {
  if (run_setting == "test") {
    n_chains = 1
    n_burnin = 2
    n_iter = 5
    n_thin = 1
  }
  if (run_setting == "short") {
    n_chains = 16
    n_burnin = 4
    n_iter = 10
    n_thin = 2
  }
  if (run_setting == "quick") {
    n_chains = 20
    n_burnin = 250
    n_iter = 500
    n_thin = 5
  }
  if (run_setting == "long") {
    n_chains = 20
    n_burnin = 500
    n_iter = 1000
    n_thin = 10
  }
  if (run_setting == "long2") {
    n_chains = 16
    n_burnin = 5000
    n_iter = 10000
    n_thin = 20
  }
  
  settings <- list(
    n_chains = n_chains,
    n_burnin = n_burnin,
    n_iter = n_iter,
    n_thin = n_thin
  )
  return(settings)
}