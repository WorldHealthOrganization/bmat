
write_model_bmat_onecountry <- function(
  jags_list,
  main_path
){
#----------------------------
  
  # start model file
  cat("model {", sep="", append=FALSE, file=here::here(main_path, "model.txt"), fill=TRUE)
  #----------------------------------


  # data models
  # if (length(jags_list$mat.j) > 0){ # this causes a bug because calling $mat.j will call mat.jinq when mat.j is NULL ... do tibbles fix this?
  if (length(jags_list[["mat.j"]]) > 0){
    cat("
   #----------------------------
   # index j for standard vr
    for (j in 1:J){
         mat.j[j] ~ dnegbin( pnegbin.j[j] , r.j[j] )
         pnegbin.j[j] <- r.j[j]/(m.j[j]+r.j[j])
         r.j[j] <-  m.j[j]^2/(v.j[j]-m.j[j])
    # mean and variance
         m.j[j] <- meanpropvr.j[j]*env.j[j]
         v.j[j] <- m.j[j] +
            env.j[j]^2*
        #----------
        ## choose one: 
        
        ## run w/o max:
        (
        
        ## run with max:
        # max(meanpropvr.j[j]^2, 
        #----------
        
        
        ## comment: vmisc/env^2
              pmtrue.j[j]^2*var_sens.j[j]
              +  (1-pmtrue.j[j])^2*var_spec.j[j]
              -  2*pmtrue.j[j]*(1-pmtrue.j[j])*cov_sesp.j[j]
          
          ## comment: v_incomplete/env^2    
          + pmtrue.j[j]^2*
            varfrombeta.j[j]*
   (sens.j[j] - (1-spec.j[j]))^2
   #(sens_sq.j[j] + oneminspec_sq.j[j])
   ) # end inside env^2, and, if applied, max statement 
   
 
        meanpropvr.j[j] <- (pvr.j[j])
        pmtrue.j[j] <- R.ct[getc.j[j], gett.j[j]]
        pmvrtrue.j[j] <- pmtrue.j[j]
        pvr.j[j] <- sens.j[j]*pmvrtrue.j[j] + (1-spec.j[j])*(1-pmvrtrue.j[j])
      }
    #------
  ",sep="", append=TRUE, file=here::here(main_path, "model.txt"), fill=TRUE)
  }
  if (length(jags_list$logpm.jnew) > 0){
    cat("

    #----
    # data source 3
    for (j in 1:Jnew){
      logpm.jnew[j] ~ dnorm(mean.jnew[j], tau.jnew[j])
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
    for (j in 1:Jnew){
      nonaidsmatoverall.jnew[j] <- (
        inprod(partialwhoenv.xjnew[1:X.jnew[j],j], nonaidsmatoverall.ct[getc.jnew[j], gettstart.jnew[j]:gettend.jnew[j]])/
          sum(partialwhoenv.xjnew[1:X.jnew[j],j])
        #partialenv = length of interval*E.ct
      )
    }

    for (c in 1:C){
      oneminpi.c[c] ~ dnorm(oneminpi.hat.c[c], 1/0.05^2)T(0,1)
      oneminpi.hat.c[c] <- isssa.c[c]*0.9 + (1-isssa.c[c])*0.85
    }
    #------
  ",sep="", append=TRUE, file=here::here(main_path, "model.txt"), fill=TRUE)
  }
  if (length(jags_list$mat.jinq) > 0){
    cat("
      # specialized studies
      for(j in 1:Jinq){
        mat.jinq[j] ~ dbinom(meanprop.jinq[j], env.jinq[j])
        meanprop.jinq[j] <- (
          inprod(partialwhoenv.xjinq[1:X.jinq[j],j], R.ct[getc.jinq[j], gettstart.jinq[j]:gettend.jinq[j]])/
              sum(partialwhoenv.xjinq[1:X.jinq[j],j])
            )
      }
  #------
  ",sep="", append=TRUE, file=here::here(main_path, "model.txt"), fill=TRUE)
  }
  if (length(jags_list$mat.g) > 0){
    cat("
    # inquiries without complete envelope
    for(g in 1:Ginq_incomplete){
      mat.g[g] ~ dnegbin(pnegbin.g[g], r.g[g])
      pnegbin.g[g] <- r.g[g]/(m.g[g]+r.g[g])

      meanprop_incomplete.g[g] <- (
          inprod(partialwhoenv.xg[1:X.g[g],g], R.ct[getc.g[g],
          gettstart.g[g]:gettend.g[g]])/
              sum(partialwhoenv.xg[1:X.g[g],g])
            )

      m.g[g] <- meanprop_incomplete.g[g]*
                    env.g[g]
      r.g[g] <-  m.g[g]^2/(v.g[g]-m.g[g])



      v.g[g] <- (m.g[g] +
               env.g[g]^2*
                  meanprop_incomplete.g[g]^2*
                  varfrombeta.g[g])
      }

      ",sep="", append=TRUE, file=here::here(main_path, "model.txt"), fill=TRUE)


  }

  # process model: arma part
  cat("
    #--------------------------
    # ARMA part
    tau.lambda <- pow(sigma.lambda,-2)
    for (c in 1:C){
      gamma0.c[c] <- pow(sqrtgamma0.c[c],2)
      tau.ar.c[c] <- pow(sigma.ar.c[c],-2)
      arma1.c[c] ~ dnorm(0, 1/(gamma0.c[c]))
      arma.ct[c,1] <- arma1.c[c]
      e.ct[c,1] ~ dnorm(sigma.ar.c[c]^2/gamma0.c[c]*arma.ct[c,1], 1/(sigma.ar.c[c]^2*(1-sigma.ar.c[c]^2/gamma0.c[c])))
      for (t in 2:nyears){
        e.ct[c,t] ~ dnorm(0, tau.ar.c[c])
        arma.ct[c,t] <- phi*arma.ct[c,t-1] - theta*e.ct[c,t-1] + e.ct[c,t]
      }
      sigma.ar.c[c] <- sqrt(gamma0.c[c] /((1-2*phi*theta + theta^2)/(1-phi^2)))
      sqrtgamma0.c[c] <- sqrtgamma0*(1+lambda.c[c])
      lambda.c[c] ~ dnorm(0, tau.lambda)T(-1,2)
    }
   ",sep="", append=TRUE, file=here::here(main_path, "model.txt"), fill=TRUE)

  # multilevel model for PM^NA
  cat("
    #----------------------------------
    # multilevel model for PM^NA
    for (c in 1:C){
      for (t in 1:nyears){
        # note negative and positive signs
        logRstarnonaids.ct[c,t] <- (- beta.h[1]*X.cth[c,t,1] + beta.h[2]*X.cth[c,t,2] - beta.h[3]*X.cth[c,t,3]
                                + alpha.c[c]
            )
        input1.ct[c,t] ~ dinterval(exp(logRstarnonaids.ct[c,t]), dintervalbounds)
      }
      alpha.c[c] ~ dnorm(alpha.r[getr.c[c]], tau.country)
    } # end C-loop
    tau.country <- pow(sigma.country, -2)


  ",sep="", append=TRUE, file=here::here(main_path, "model.txt"), fill=TRUE)


  #----------------------------------
  # putting things together
  cat("
    # putting things together
    for (c in 1:C){
      for (t in 1:nyears){
        Rstarnonaids.ct[c,t] <- exp(logRstarnonaids.ct[c,t])
        munonaidsstar.ct[c,t] <- Rstarnonaids.ct[c,t]/B.ct[c,t]*E.ct[c,t]*(1-a.ct[c,t])
        logmunonaidsstar.ct[c,t]<-log(munonaidsstar.ct[c,t])
      }
    }
  ",sep="", append=TRUE, file=here::here(main_path, "model.txt"), fill=TRUE)

  cat("
     #----------------------------------
   for (c in 1:C){
      for (t in 1:nyears){
        mustar.ct[c,t] <- munonaidsstar.ct[c,t] + muaids.ct[c,t]
        munonaids.ct[c,t]<-exp(logmunonaids.ct[c,t])
        mu.ct[c,t] <-munonaids.ct[c,t] + muaids.ct[c,t]
        R.ct[c,t] <- mu.ct[c,t]*B.ct[c,t]/E.ct[c,t]
        nonaidsmatoverall.ct[c,t] <- (mu.ct[c,t]-muaids.ct[c,t])*B.ct[c,t]/E.ct[c,t]
      }
      logmunonaids.ct[c,year.ref]<-logmunonaidsstar.ct[c,year.ref]
      for (t in (year.ref+1):nyears){
        logmunonaids.ct[c,t] <- (logmunonaids.ct[c,t-1]+arma.ct[c,t-1]
                  +logmunonaidsstar.ct[c,t]-logmunonaidsstar.ct[c,t-1])
      }
    }# end c loop, before ref year is further below in code

    for (c in 1:C){
      for (t in 1:nyears){
        input1again.ct[c,t] ~ dinterval(R.ct[c,t], dintervalbounds)
      }
    }

  ",sep="", append=TRUE, file=here::here(main_path, "model.txt"), fill=TRUE)

    # loop to get results for t < reference year (relevant only when reference year > 1985)
    cat("
      for (c in 1:C){
        for (t in 1:(year.ref-1)){
        logmunonaids.ct[c,t] <- (logmunonaids.ct[c,t+1]-arma.ct[c,t]
                - (logmunonaidsstar.ct[c,t+1]-logmunonaidsstar.ct[c,t]))
        }
      } # end c-loop

  ",sep="", append=TRUE, file=here::here(main_path, "model.txt"), fill=TRUE)

  #----------------------------------
  # end model
  cat("
  #----------------------------------
  } # end model
  ",sep="", append=TRUE, file=here::here(main_path, "model.txt"), fill=TRUE)

} # end function


