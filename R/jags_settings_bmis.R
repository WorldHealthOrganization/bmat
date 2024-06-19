jags_settings_bmis <- function(run_setting) {
  if (run_setting == "test") {
    n_chains = 2
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
  if (run_length == "quick_local") {
    n_chains <- 8
    n_iter <- 375
    n_thin <- 3
    n_burnin <- 200
  }
  # if (run_length == "quick_local") {
  #   n_chains <- 8
  #   n_iter <- 500
  #   n_thin <- 4
  #   n_burnin <- 500
  # }
  if (run_setting == "quick") {
    n_chains = 20
    n_burnin = 200
    n_iter = 200
    n_thin = 4
  }
  if (run_setting == "long_local") {
    n_chains = 8
    n_burnin = 500
    n_iter = 2500
    n_thin = 20
  }
  if (run_setting == "long") {
    n_chains = 20
    n_burnin = 500
    n_iter = 1000
    n_thin = 20
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