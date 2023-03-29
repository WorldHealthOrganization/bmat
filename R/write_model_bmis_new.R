
write_model_new <- function(dmnames, jags_list, global_run, main_path) {

  
  if (file.exists(here::here(main_path, "model.txt"))) {
    file.remove(here::here(main_path, "model.txt"))
  }
  
  JAGSmodel.name <- "model.txt"

  if (length(jags_list$overlap.dm) !=0){
  cat("
data{
  for(j in 1:length(invr)){
         zeroes_j[j] <- 0
  }
}

", sep = "", append = FALSE, file = here::here(main_path, "model.txt"), fill = TRUE)
  }

cat("
model {",
    sep = "",
    append = TRUE,
    file = here::here(main_path, "model.txt"),
    fill = TRUE
  )


  cat("
for(c in 1:C){
  for(t in 1:nyears){
  gamma.truematvr.ct[c,t] ~ dunif(0.0001, 1)
  beta.ct[c,t]  <- 0
  } #end t loop


#Get mean level for eta's based on hierarchical model
#Use Sigma World here to inform first level variance


mean_eta.ck[c,1] ~ dnorm(etaworld.k[1], 1/(sigmaworld1^2))
cond_mean_eta.c[c] <- etaworld.k[2] + sigmaworld2/sigmaworld1*rho_world*(mean_eta.ck[c,1] - etaworld.k[1])
mean_eta.ck[c, 2] ~ dnorm(cond_mean_eta.c[c],  1/(sigmaworld2^2 *(1-rho_world^2)))




#c-t specific eta's are mean level + delta deviations
# eta.ctk[c,1:nyears,1] = mean_eta.ck[c,1] + Dcomb %*% delta.cmk[c,1:(nyears-1),1]
# eta.ctk[c,1:nyears,2] = mean_eta.ck[c,2] + Dcomb %*% delta.cmk[c,1:(nyears-1),2]


for(t in 1:nyears){
 eta.ctk[c,t,1] = mean_eta.ck[c,1] + Dcomb[t,] %*% delta.cmk[c,1:(nyears-1),1]
 eta.ctk[c,t,2] = mean_eta.ck[c,2] + Dcomb[t,] %*% delta.cmk[c,1:(nyears-1),2]
}


#Deltas are deviations away from mean, modeled bivar normal
for(tindex in  1:(nyears-1)){
mean_delta.cmk[c,tindex,1:K] <- c(0,0)
delta.cmk[c,tindex,1] ~ dnorm(mean_delta.cmk[c,tindex,1], prec_delta)
cond_mean_delta.cmk[c,tindex] <- mean_delta.cmk[c,tindex,2] + sigma.beta/sigma.alpha*rho.alphabeta*(delta.cmk[c,tindex,1] - mean_delta.cmk[c,tindex,1])
delta.cmk[c,tindex,2] ~ dnorm(cond_mean_delta.cmk[c,tindex],  condprec_delta)
}


#Transform etas to original scale (se and sp)
for(t in 1:nyears){
  # sens.ct[c,t] <- 1/(1 + exp(-eta.ctk[c,t,1]))
  # spec.ct[c,t] <- 1/(1 + exp(-eta.ctk[c,t,2]))
  
  sens.ct[c,t] <- phi(eta.ctk[c,t,1])
  spec.ct[c,t] <- phi(eta.ctk[c,t,2])
}

} #end C loop


prec_delta <- 1/sigma.alpha^2
condprec_delta <- 1/(sigma.beta^2 *(1-rho.alphabeta^2))


", sep = "", append = TRUE, file = here::here(main_path, "model.txt"), fill = TRUE)


if (global_run ) {
  cat("
rho.alphabeta ~ dunif(-1,1)
sigma.alpha ~ dnorm(0, 1)T(0,)
sigma.beta ~ dnorm(0, 1)T(0,)



# back to uniform on world means
sensworld ~ dunif(0,1)
specworld ~ dunif(0,1)

# etaworld.k[1] <- log(sensworld/(1 - sensworld))
# etaworld.k[2] <- log(specworld/(1 - specworld))

etaworld.k[1] <- probit(sensworld)
etaworld.k[2] <- probit(specworld)

# set large variance for prior but fix rho at value from country etas
sigmaworld1 ~ dnorm(0,1)T(0,)
sigmaworld2 ~ dnorm(0,1)T(0,)
rho_world ~ dunif(-1,1)


        ", sep = "", append = TRUE, file = here::here(main_path, "model.txt"), fill = TRUE)
  }

  #--------------- end RW

  #--------------------- Reparametrize
  #----------------------------------------------------------------------------------------------------------------------


  cat("

    for (c in 1:C){
    for (t in 1:nyears){

    # also calculate vr adjustment
    # based on estimated gammamatinvr (based on observed may be an issue so use function in R)
    vradj.ct[c,t] <- (rho.ctb[c,t,fn_index] + rho.ctb[c,t,tp_index] + rho.ctb[c,t,up_index])/gamma.matvr.ct[c,t]

    #Use below if parametrizing in terms of gamma.matvr.ct
   gamma.matvr.ct[c,t] <- sens.ct[c,t]*gamma.truematvr.ct[c,t] + (1-spec.ct[c,t])*(1-gamma.truematvr.ct[c,t])


    gamma.ctb[c,t,tp_index] =  sens.ct[c,t] * gamma.truematvr.ct[c,t]
    gamma.ctb[c,t,fn_index] = gamma.truematvr.ct[c,t] - gamma.ctb[c,t,tp_index]
    gamma.ctb[c,t,tn_index]  = spec.ct[c,t]* (1-gamma.truematvr.ct[c,t])
    gamma.ctb[c,t,fp_index] = (1-gamma.truematvr.ct[c,t])- gamma.ctb[c,t,tn_index]


    rho.ctb[c,t,tp_index] =  gamma.ctb[c,t,tp_index] * rhovr.ct[c,t]
    rho.ctb[c,t,fn_index] = gamma.ctb[c,t,fn_index]* rhovr.ct[c,t] 
    rho.ctb[c,t,tn_index]  = gamma.ctb[c,t,tn_index] * rhovr.ct[c,t]
    rho.ctb[c,t,fp_index] = gamma.ctb[c,t,fp_index] * rhovr.ct[c,t]



   # if beta represents the logodds, then formula for alpha changes
    odds.ct[c,t] <- exp(beta.ct[c,t])
    #alpha.ct[c,t] <- some function of odds and gamma.truematvr.ct[c,t] #alpha = PMout
    # see test_logodds.R
    alpha.ct[c,t] <- gamma.truematvr.ct[c,t]*odds.ct[c,t]/((1-gamma.truematvr.ct[c,t]) + odds.ct[c,t]*gamma.truematvr.ct[c,t])
    rho.ctb[c,t,up_index] = alpha.ct[c,t] * (1-rhovr.ct[c,t])
    rho.ctb[c,t,un_index] = 1 - rhovr.ct[c,t] - rho.ctb[c,t,up_index]
    }
    }

    ",
    sep = "",
    append = TRUE,
    file = here::here(main_path, "model.txt"),
    fill = TRUE
  )



  #--------------------- Var-Cov matrix
  #----------------------------------------------------------------------------------------------------------------------


  cat("


   # define the varcov matrix for the multnomial for all c,t

    for(c in 1:C){
    for(t in 1:nyears){
    for (b in 1:Binvr){

    Sigma.single.ctbb[c,t,b,b] <- gamma.ctb[c,t,b]*(1-gamma.ctb[c,t,b])/ytot_vr.ct[c,t]

    }
    for (b1 in 1:(Binvr-1)){
    for (b2 in (b1+1):Binvr){

    Sigma.single.ctbb[c,t,b1,b2] <- -(gamma.ctb[c,t,b1]*gamma.ctb[c,t,b2])/ytot_vr.ct[c,t]
    Sigma.single.ctbb[c,t,b2,b1] <- Sigma.single.ctbb[c,t,b1,b2]

    }} #end binvr loop
    }#end t loop
    } #end c loop
 ",
    sep = "",
    append = TRUE,
    file = here::here(main_path, "model.txt"),
    fill = TRUE
  )

  if ("tn + fn" %in% dmnames) {
    cat("


    ### Model single year data of truemattot, fn tn using normal (CRI and SUR)

        #For now we just model tn using binom out of total negative maternal, but for more correct version, code started below (needs to be fixed/checked)

       for(m in tnbinom.dm){
        for(j in 1:nobs.m[m]){
        logliketn.mj[m,j] <- logdensity.multi(  multitnfnmatvr.mjs[m,j, 1:3],mutnfn.mjs[m,j, 1:3], ztot_vr.mj[m,j])
        multitnfnmatvr.mjs[m,j, 1:3] ~ dmulti(mutnfn.mjs[m,j, 1:3],  ztot_vr.mj[m,j])



        gammatn.mj[m,j] <- gamma.ctb[getc.mj[m,j], getmidyear.mj[m,j], tn_index]
        gammafn.mj[m,j] <- gamma.ctb[getc.mj[m,j], getmidyear.mj[m,j], fn_index] 
        gammamatvr.mj[m,j] <- sum(gamma.ctb[getc.mj[m,j], getmidyear.mj[m,j],vrmat_index ])
       mutnfn.mjs[m,j,1:3] <- c(gammatn.mj[m,j], gammafn.mj[m,j], gammamatvr.mj[m,j])

        }
        }

", sep = "", append = TRUE, file = here::here(main_path, "model.txt"), fill = TRUE)
  }


  #--------------------- data models
  #----------------------------------------------------------------------------------------------------------------------
#if (length(jags_list$overlap.dm)>0){
  if(sum(c("overlapinvr.singleyrs", "overlapinvr.multipleyrs", "overlapoutvr.singleyrs", "overlapoutvr.multipleyrs") %in% dmnames)>0){

  cat("


  for(m in overlap.dm){
    for(j in 1:length(include)){
    for(z in 1:Z.j[include[j]]){
    #contrL_invr[j,z] <- logdensity.multi(


    contrL_invr[j,z] <- exp(contrLL_invr[j,z])
    contrLL_invr[j,z] <- logdensity.multi(

      c(inputarray.jzs[include[j], z, tn_index],
      inputarray.jzs[include[j], z, fn_index],
      inputarray.jzs[include[j], z, fp_index],
      inputarray.jzs[include[j], z, tp_index]),

      #input specific 4 proportion parameters
      c(gamma.jzs[j, z, vr_index]),  sum(inputarray.jzs[include[j], z , 1: Sinvr])) #end log density

      gamma.jzs[j, z, tn_index] <- rho.ctb[getc.mj[m,include[j]], getmidyear.mj[m,include[j]], tn_index] / sum(rho.ctb[getc.mj[m,include[j]], getmidyear.mj[m,include[j]], vr_index])
      gamma.jzs[j, z, fn_index] <- rho.ctb[getc.mj[m,include[j]], getmidyear.mj[m,include[j]], fn_index] / sum(rho.ctb[getc.mj[m,include[j]], getmidyear.mj[m,include[j]], vr_index])
      gamma.jzs[j, z, fp_index] <- rho.ctb[getc.mj[m,include[j]], getmidyear.mj[m,include[j]], fp_index] / sum(rho.ctb[getc.mj[m,include[j]], getmidyear.mj[m,include[j]], vr_index])
      gamma.jzs[j, z, tp_index] <- rho.ctb[getc.mj[m,include[j]], getmidyear.mj[m,include[j]], tp_index] / sum(rho.ctb[getc.mj[m,include[j]], getmidyear.mj[m,include[j]], vr_index])
    }#end z loop

    #LL_j[j] <- sum(contrL_invr[j, 1:Z.j[include[j]]])
    LL_j[j] <- log(sum(contrL_invr[j, 1:Z.j[include[j]]]) + 0.000000000000000001)
    }#end j loop


      for(j in 1:length(include)){
      #pois.mean_j[j] <- -(LL_j[j] + 100000000)
      pois.mean_j[j] <- -(LL_j[j]) + log(Z.j[include[j]]) +1
      zeroes_j[j] ~ dpois(pois.mean_j[j])
      }


    }



    ",
    sep = "",
    append = TRUE,
    file = here::here(main_path, "model.txt"),
    fill = TRUE
  )
}


  if ("tp + fp" %in% dmnames | "fp" %in% dmnames) {
    cat("
 ### Model single year data of fp out of Matvr (NOR)


      for(m in fpbinom.dm){
      for(j in 1:nobs.m[m]){
      loglikefp.mj[m,j] <- logdensity.multi(  multifptpnonmatvr.mjs[m,j, 1:3], mufptp.mjs[m,j, 1:3], ztot_vr.mj[m,j])
      multifptpnonmatvr.mjs[m,j, 1:3] ~ dmulti( mufptp.mjs[m,j,1:3], ztot_vr.mj[m,j])

    gammafp1.mj[m,j] <- rho.ctb[getc.mj[m,j], getmidyear.mj[m,j], fp_index] / sum(rho.ctb[getc.mj[m,j], getmidyear.mj[m,j], vr_index])
    gammatp1.mj[m,j] <- rho.ctb[getc.mj[m,j], getmidyear.mj[m,j], tp_index] / sum(rho.ctb[getc.mj[m,j], getmidyear.mj[m,j], vr_index])
    gammanonmatvr1.mj[m,j] <- sum(rho.ctb[getc.mj[m,j], getmidyear.mj[m,j], indicesnonmatvr]) / sum(rho.ctb[getc.mj[m,j], getmidyear.mj[m,j], vr_index])

    mufptp.mjs[m,j, 1:3] <- c(gammafp1.mj[m,j], gammatp1.mj[m,j], gammanonmatvr1.mj[m,j])
      }
      }



", sep = "", append = TRUE, file = here::here(main_path, "model.txt"), fill = TRUE)
  }





  if ("Multinom.outVR" %in% dmnames) {
    cat("
#### Model multinomial for all ordered by c(fn, fp, tp, tn up, un)  for FRA and CUBA studies, France is single year, Cuba is multiple years


      for(m in multioutvr.dm){
      for(j in 1:nobs.m[m]){

      loglikemultioutvr.mj[m,j] <- logdensity.multi(multioutvrcounts.mjs[m,j, 1:Soutvr],mumulti.mjs[m,j,1:Soutvr], ztot.mj[m,j])
      multioutvrcounts.mjs[m,j, 1:Soutvr] ~ dmulti(mumulti.mjs[m,j,1:Soutvr], ztot.mj[m,j])



      rhomulti.mjs[m,j,fn_index] <- rho.ctb[getc.mj[m,j], getmidyear.mj[m,j], fn_index]
      rhomulti.mjs[m,j,fp_index] <- rho.ctb[getc.mj[m,j], getmidyear.mj[m,j], fp_index]
      rhomulti.mjs[m,j,tp_index] <- rho.ctb[getc.mj[m,j], getmidyear.mj[m,j], tp_index]
      rhomulti.mjs[m,j,tn_index] <- rho.ctb[getc.mj[m,j], getmidyear.mj[m,j], tn_index]
      rhomulti.mjs[m,j,up_index] <- rho.ctb[getc.mj[m,j], getmidyear.mj[m,j], up_index]
      rhomulti.mjs[m,j,un_index] <- rho.ctb[getc.mj[m,j], getmidyear.mj[m,j], un_index]

      #Sum of rhos=1, so no need to have denominator
      mumulti.mjs[m,j,1:Soutvr] <- rhomulti.mjs[m,j,1:Soutvr]
      }
      }

", sep = "", append = TRUE, file = here::here(main_path, "model.txt"), fill = TRUE)
  }










  if ("Multinom.inVR" %in% dmnames) {
    cat("
        #### Model multinomial for all ordered by c(fn, fp, tp, tn )


        for(m in multiinvr.dm){
        for(j in 1:nobs.m[m]){

        loglikemultiinvr.mj[m,j] <- logdensity.multi(multiinvrcounts.mjs[m,j, 1:Sinvr], mumultiinvr.mjs[m,j,1:Sinvr], ztot_vr.mj[m,j])
        multiinvrcounts.mjs[m,j, 1:Sinvr] ~ dmulti(mumultiinvr.mjs[m,j,1:Sinvr], ztot_vr.mj[m,j])


        gammamultiinvr.mjs[m,j,fn_index] <- gamma.ctb[getc.mj[m,j], getmidyear.mj[m,j], fn_index]
        gammamultiinvr.mjs[m,j,fp_index] <- gamma.ctb[getc.mj[m,j], getmidyear.mj[m,j], fp_index]
        gammamultiinvr.mjs[m,j,tp_index] <- gamma.ctb[getc.mj[m,j], getmidyear.mj[m,j], tp_index]
        gammamultiinvr.mjs[m,j,tn_index] <- gamma.ctb[getc.mj[m,j], getmidyear.mj[m,j], tn_index]

        #Sum of rhos=1, so no need to have denominator
        mumultiinvr.mjs[m,j,1:Sinvr] <- gammamultiinvr.mjs[m,j,1:Sinvr]
        }
        }

        ", sep = "", append = TRUE, file = here::here(main_path, "model.txt"), fill = TRUE)
  }









  cat("
    #   ----------------------------------
    }  #end model
    ",
    sep = "",
    append = TRUE,
    file = here::here(main_path, "model.txt"),
    fill = TRUE
  )
  return(NULL)
}
