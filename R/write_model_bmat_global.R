
write_model_bmat_global <- function(
  main_path
){
  
    cat("
    model {
  #-----------------
  # data models
  #-----------------
  # 1. standard vr
  for (g in 1:Gtrain){
    mat.j[getj.gtrain[g]] ~ dnegbin( pnegbin.j[getj.gtrain[g]] , r.j[getj.gtrain[g]] )
    pnegbin.j[getj.gtrain[g]] <- r.j[getj.gtrain[g]]/(m.j[getj.gtrain[g]]+r.j[getj.gtrain[g]])
    r.j[getj.gtrain[g]] <-  m.j[getj.gtrain[g]]^2/(v.j[getj.gtrain[g]]-m.j[getj.gtrain[g]])
    m.j[getj.gtrain[g]] <- meanpropvr.j[getj.gtrain[g]]*env.j[getj.gtrain[g]]
    v.j[getj.gtrain[g]] <- m.j[getj.gtrain[g]] +
            env.j[getj.gtrain[g]]^2*
        #----------
        ## choose one: 
        
        ## run w/o max:
        (
        
        ## run with max:
        # max(meanpropvr.j[getj.gtrain[g]]^2, 
        #----------
        
        
        ## comment: vmisc/env^2
              pmtrue.j[getj.gtrain[g]]^2*var_sens.j[getj.gtrain[g]]
              +  (1-pmtrue.j[getj.gtrain[g]])^2*var_spec.j[getj.gtrain[g]]
              -  2*pmtrue.j[getj.gtrain[g]]*(1-pmtrue.j[getj.gtrain[g]])*cov_sesp.j[getj.gtrain[g]]
          
          ## comment: v_incomplete/env^2    
          + pmtrue.j[getj.gtrain[g]]^2*
            varfrombeta.j[getj.gtrain[g]]*
   (sens.j[getj.gtrain[g]] - (1-spec.j[getj.gtrain[g]]))^2
   #(sens_sq.j[getj.gtrain[g]] + oneminspec_sq.j[getj.gtrain[g]])
   ) # end inside env^2, and, if applied, max statement 
  }
  # 2. specialized studies, complete envelope
  for(g in 1:Ginqtrain){
    mat.jinq[getjinq.ginqtrain[g]] ~ dbinom(meanprop.jinq[getjinq.ginqtrain[g]],
                                            env.jinq[getjinq.ginqtrain[g]])
  }
  # specialized studies without complete envelope
  for(g in 1:Ginqtrain_incomplete){
    mat_incomplete.jinq[getjinq_incomplete.ginqtrain[g]] ~ dnegbin(pnegbin.g[g], r.g[g])
    pnegbin.g[g] <- r.g[g]/(m.g[g]+r.g[g])
    m.g[g] <- meanprop_incomplete.jinq[getjinq_incomplete.ginqtrain[g]]*
      env_incomplete.jinq[getjinq_incomplete.ginqtrain[g]]
    r.g[g] <-  m.g[g]^2/(v.g[g]-m.g[g])
    v.g[g] <- (m.g[g] +
                 env_incomplete.jinq[getjinq_incomplete.ginqtrain[g]]^2*
                 meanprop_incomplete.jinq[getjinq_incomplete.ginqtrain[g]]^2*
                 varfrombeta.jinq[getjinq_incomplete.ginqtrain[g]])
  }
  # 3. other studies
  for (g in 1:Gnewtrain){
    logpm.jnew[getjnew.gnewtrain[g]] ~ dnorm(mean.jnew[getjnew.gnewtrain[g]],
                                             tau.jnew[getjnew.gnewtrain[g]])
  }
  
  # additional information, for all indices, including those in test set
  # for VR data
  for (j in 1:J){
    meanpropvr.j[j] <- (pvr.j[j])
    pmtrue.j[j] <- R.ct[getc.j[j], gett.j[j]]
    pmvrtrue.j[j] <- pmtrue.j[j]
    pvr.j[j] <- sens.j[j]*pmvrtrue.j[j] + (1-spec.j[j])*(1-pmvrtrue.j[j])
  }
  
  # for data source 3: other
  nonsamplingdhs.se ~ dunif(0.1, 0.5)
  nonsamplingnondhs.se ~ dunif(0.1, 0.5)
  for (j in 1:Jnew){
    tau.jnew[j] <- 1/(1/tausamp.jnew[j]
                      + ismisc.jnew[j]*
                        (isdhs.jnew[j]*nonsamplingdhs.se^2+(1-isdhs.jnew[j])*nonsamplingnondhs.se^2))
    mean.jnew[j] <- (logphi.jnew[j]      +
                       log(nonaidsmatoverall.jnew[j]*
                             ( (1-ispreg.jnew[j]) + phi.preg.jnew[j]*ispreg.jnew[j] )
                           + uaids*aidsva.jnew[j] #for all, total for maternal
                           +(1-uaids)*aidsva.jnew[j]*ispreg.jnew[j]
                       ))
    phi.preg.jnew[j] <- 1/oneminpi.c[getc.jnew[j]]
    logphi.jnew[j] <- log(1/(ismisc.jnew[j]*1.1 + isinq.jnew[j]*1 ))
  }
  # for specialized studies
  for (j in 1:Jinq){
    meanprop.jinq[j] <- (matoverall.jinq[j])
    matoverall.jinq[j] <- (
      inprod(partialwhoenv.xjinq[1:X.jinq[j],j],
             R.ct[getc.jinq[j], gettstart.jinq[j]:gettend.jinq[j]])/
        sum(partialwhoenv.xjinq[1:X.jinq[j],j]))
  }
  for (j in 1:Jinq_incomplete){
    meanprop_incomplete.jinq[j] <- (matoverall_incomplete.jinq[j])
    matoverall_incomplete.jinq[j] <- (
      inprod(partialwhoenv_incomplete.xjinq[1:X_incomplete.jinq[j],j],
             R.ct[getc_incomplete.jinq[j], gettstart_incomplete.jinq[j]:gettend_incomplete.jinq[j]])/
        sum(partialwhoenv_incomplete.xjinq[1:X_incomplete.jinq[j],j])
    )
  }
  
  # data sources 3
  for (j in 1:Jnew){
    nonaidsmatoverall.jnew[j] <- (
      inprod(partialwhoenv.xjnew[1:X.jnew[j],j],
             nonaidsmatoverall.ct[getc.jnew[j], gettstart.jnew[j]:gettend.jnew[j]])/
        sum(partialwhoenv.xjnew[1:X.jnew[j],j])
    )
  }
  for (c in 1:C){
    oneminpi.c[c] ~ dnorm(oneminpi.hat.c[c], 1/0.05^2)T(0,1)
    oneminpi.hat.c[c] <- isssa.c[c]*0.9 + (1-isssa.c[c])*0.85
  }
  
  #-----------------
  # ARMA part
  #-----------------
  phi ~ dunif(0,1)
  theta <- 0
  sqrtgamma0 ~ dunif(0,0.025)
  tau.lambda <- pow(sigma.lambda,-2)
  sigma.lambda ~ dunif(0,max.sigma.lambda)
  for (c in 1:C){
    gamma0.c[c] <- pow(sqrtgamma0.c[c],2)
    tau.ar.c[c] <- pow(sigma.ar.c[c],-2)
    arma1.c[c] ~ dnorm(0, 1/(gamma0.c[c]))
    arma.ct[c,1] <- arma1.c[c]
    e.ct[c,1] ~ dnorm(sigma.ar.c[c]^2/gamma0.c[c]*arma.ct[c,1],
                      1/(sigma.ar.c[c]^2*(1-sigma.ar.c[c]^2/gamma0.c[c])))
    for (t in 2:nyears){
      e.ct[c,t] ~ dnorm(0, tau.ar.c[c])
      arma.ct[c,t] <- phi*arma.ct[c,t-1] - theta*e.ct[c,t-1] + e.ct[c,t]
    }
    sigma.ar.c[c] <- sqrt(gamma0.c[c] /((1-2*phi*theta + theta^2)/(1-phi^2)))
    sqrtgamma0.c[c] <- sqrtgamma0*(1+lambda.c[c])
    lambda.c[c] ~ dnorm(0, tau.lambda)T(-1,2)
  }
  
  #----------------------------------
  # multilevel model for PM^NA
  #-----------------
  for (c in 1:C){
    for (t in 1:nyears){
      logRstarnonaids.ct[c,t] <- (- beta.h[1]*X.cth[c,t,1] +
                                    beta.h[2]*X.cth[c,t,2] - beta.h[3]*X.cth[c,t,3]
                                  + alpha.c[c])
      input1.ct[c,t] ~ dinterval(exp(logRstarnonaids.ct[c,t]), dintervalbounds)
    }
    alpha.c[c] ~ dnorm(alpha.r[getr.c[c]], tau.country)
  } # end C-loop

  tau.country <- pow(sigma.country, -2)
  for (r in 1:R){
    alpha.r[r] ~ dnorm(alpha.world, tau.region)
  }
  alpha.world ~ dnorm(log(0.001), 0.01)
  tau.region <- pow(sigma.region, -2)
  sigma.region ~ dunif(0, 5)
  sigma.country ~ dunif(0, 5)
  for (h in 1:3){
    beta.h[h] ~ dnorm(0.5, 0.001)
  }
  #---
  # putting things together
  #-----------------
  for (c in 1:C){
    for (t in 1:nyears){
      Rstarnonaids.ct[c,t] <- exp(logRstarnonaids.ct[c,t])
      munonaidsstar.ct[c,t] <- Rstarnonaids.ct[c,t]/B.ct[c,t]*E.ct[c,t]*(1-a.ct[c,t])
      logmunonaidsstar.ct[c,t]<-log(munonaidsstar.ct[c,t])
      mustar.ct[c,t] <- munonaidsstar.ct[c,t] + muaids.ct[c,t]
      munonaids.ct[c,t]<-exp(logmunonaids.ct[c,t])
      mu.ct[c,t] <-munonaids.ct[c,t] + muaids.ct[c,t]
      R.ct[c,t] <- mu.ct[c,t]*B.ct[c,t]/E.ct[c,t]
      nonaidsmatoverall.ct[c,t] <- (mu.ct[c,t]-muaids.ct[c,t])*B.ct[c,t]/E.ct[c,t]
      input1again.ct[c,t] ~ dinterval(R.ct[c,t], dintervalbounds)
    }
    logmunonaids.ct[c,year.ref]<-logmunonaidsstar.ct[c,year.ref]
    for (t in (year.ref+1):nyears){
      logmunonaids.ct[c,t] <- (logmunonaids.ct[c,t-1]+arma.ct[c,t-1]
                               +logmunonaidsstar.ct[c,t]-logmunonaidsstar.ct[c,t-1])
    }
    for (t in 1:(year.ref-1)){
      logmunonaids.ct[c,t] <- (logmunonaids.ct[c,t+1]-arma.ct[c,t]
                               - (logmunonaidsstar.ct[c,t+1]-logmunonaidsstar.ct[c,t]))
    }
  }
  #----------------------------------
} # end model
", sep="", append=FALSE, file=here::here(main_path, "model.txt"), fill=TRUE)
}
