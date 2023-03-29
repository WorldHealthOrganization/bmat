pars_to_save_function <-function(global_run) {
  if(global_run) {
    pars_to_save <- c(
      "v.j",
      "sigma.alpha", "rsigma.alpha", "sensworld",
      "sens.ct",
      "arma.ct",
      "phi", "theta",
      "sqrtgamma0", "sigma.lambda","lambda.c","sigma.c",
      "logsigma.ar.ct","sigma.ar.ct", "mean.gamma", "sigma.gamma","phi.gamma",
      "mu.ct", "mustar.ct", "beta.h", "alpha.c", "alpha.r", "alpha.world", "sigma.country", "sigma.region",
      "nonsamplingdhs.se","nonsamplingnondhs.se",
      "oneminpi.c", "logphi.preg.jnew", "logphi.jnew",
      "tau.jnew",
      "tau.j",
      "p.wprior.k2")
  } else {
    pars_to_save <- c("mu.ct", "alpha.c", "lambda.c", "arma.ct", "oneminpi.c", "logphi.jnew", "tau.jnew")
  }
  return(pars_to_save)
}