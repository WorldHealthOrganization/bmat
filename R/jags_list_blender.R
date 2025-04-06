jags_list_blender <- function(jags_list_m,
                              jags_list_central,
                              specmin,
                              sensmin,
                              refyear,
                              first_year
                              ){


  B <- jags_list_central$B
  Tyears <- nyears <- jags_list_central$nyears
  J <- 400#jags_list_central$nobs # just a observation number that is larger than the each subset of data. It's okay to have an array with larger dimensions since we only loop over certain observations in actual model
  startyear <- first_year
  indices_binary_measure <- indices_binary_measure()
  list2env(indices_binary_measure, envir = environment())


  # index meta stff
  # meta info
  overlap.ind <- which(names(jags_list_m) == "overlap")
  tnbinom.ind <- which(names(jags_list_m) == "tnbinom")
  fpbinom.ind <- which(names(jags_list_m) == "fpbinom")
  fntruematvrbinom.ind <-  which(names(jags_list_m) == "fntruematvrbinom")
  fntruematbinom.ind <-  which(names(jags_list_m) == "fntruematbinom")
  multioutvr.ind <- which(names(jags_list_m) == "multinomoutvr")
  multiinvr.ind <- which(names(jags_list_m) == "multinominvr")
  multitnfnmatvr.ind <- which(names(jags_list_m) == "multinomtnfnmatvr")
  fntpmultinom.ind <- which(names(jags_list_m) == "fntpmultinom")
  truematvrbinom.ind <- which(names(jags_list_m) == "truematvrbinom")
  M <- length(jags_list_m)


  if(length(overlap.ind)>0){
    Z.n <- jags_list_m[[overlap.ind]]$Z.n
    B.n <- jags_list_m[[overlap.ind]]$B.n
    inputarray.nzs <- jags_list_m[[overlap.ind]]$inputarray.nzs
    iso_alpha_3_code.n <- jags_list_m[[overlap.ind]]$iso_alpha_3_code.n
  }

  nobs.m <- rep(NA, M)
  nyears.mj <- nmidyear.mj <- getc.mj <-  truemat.mj <- truematvr.mj <- truemattot.mj <- truematprop.mj <- ztot.mj <- ztot_vr.mj <- zmatvr.mj <- getmidyear.mj <- array(NA, c(M,J))
  fn.mj <- tn.mj <- tp.mj <-fp.mj <- un.mj <- up.mj <-env.mj <- array(NA, c(M,J))
  indicestruemat.jx <- array(NA, c(J, 3))
  nindicestruemat.j <- array(NA, J)

  for(m in 1:M){
    nobs.m[m] <-jags_list_m[[m]]$nobs

    for(j in 1:nobs.m[m]){

      nyears.mj[m,j] <- sum(!is.na(jags_list_m[[m]]$getyearindices.jt[j,]))
      nmidyear.mj[m,j] <- 1
      getc.mj[m,j] <- jags_list_m[[m]]$getc.j[j]
      getmidyear.mj[m,j] <- jags_list_m[[m]]$refyears.j[j] - startyear + 1
      truemat.mj[m,j] <- unlist(jags_list_m[[m]]$truemat.j[j])

      ztot.mj[m,j] <- unlist(jags_list_m[[m]]$ztot.j[j])
      ztot_vr.mj[m,j] <- sum(jags_list_m[[m]]$ytot_vr.jt[j,1:nyears.mj[m,j]])
      zmatvr.mj[m,j] <- sum(jags_list_m[[m]]$matvr.jt[j,1:nyears.mj[m,j]])
      truematprop.mj[m,j] <- truemat.mj[m,j]/ztot.mj[m,j]
    }
  }




  matvr.mjt <- getyears.mjt  <- ytot.mjt <- ytot_vr.mjt <- getmidyear.mjt <-array(NA, c(M,J,Tyears))

  Soutvr <- 6
  Sinvr <- 4
  S = 3
  multioutvrcounts.mjs <- array(NA, c(M,J,Soutvr))
  multiinvrcounts.mjs <- array(NA, c(M,J,Sinvr))
  multitnfnmatvr.mjs <- array(NA, c(M,J,S))
  multifntptruenonmatvr.mjs <- array(NA, c(M,J,S))

  for(m in 1:M){
    for(j in 1:nobs.m[m]){
      if(!(m %in% c(fntruematvrbinom.ind, truematvrbinom.ind))){
        matvr.mjt[m,j,1:nyears.mj[m,j]] <- jags_list_m[[m]]$matvr.jt[j,1:nyears.mj[m,j]]
        ytot.mjt[m,j,1:nyears.mj[m,j]] <- jags_list_m[[m]]$ytot.jt[j, 1:nyears.mj[m,j]]

      }

      ytot_vr.mjt[m,j,1:nyears.mj[m,j]] <- jags_list_m[[m]]$ytot_vr.jt[j, 1:nyears.mj[m,j]]
      getyears.mjt[m,j,1:nyears.mj[m,j]] <- jags_list_m[[m]]$getyearindices.jt[j, 1:nyears.mj[m,j]]
      start = jags_list_m[[m]]$getyearindices.jt[j, 1]
      end = jags_list_m[[m]]$getyearindices.jt[j, nyears.mj[m,j]]

      getmidyear.mjt[m,j,1:nyears.mj[m,j]] <-  floor((start + end)* 1/2) - startyear + 1

      fn.mj[m,j] <- jags_list_m[[m]]$fn.j[j]
      fp.mj[m,j] <- jags_list_m[[m]]$fp.j[j]
      tp.mj[m,j] <- jags_list_m[[m]]$tp.j[j]
      tn.mj[m,j] <- jags_list_m[[m]]$tn.j[j]
      up.mj[m,j] <- jags_list_m[[m]]$up.j[j]
      un.mj[m,j] <- jags_list_m[[m]]$un.j[j]
      multioutvrcounts.mjs[m,j,c(fn_index, fp_index, tp_index, tn_index, up_index, un_index)] <- c(fn.mj[m,j], fp.mj[m,j], tp.mj[m,j], tn.mj[m,j], up.mj[m,j], un.mj[m,j])
      multiinvrcounts.mjs[m,j,c(fn_index, fp_index, tp_index, tn_index)] <- c(fn.mj[m,j], fp.mj[m,j], tp.mj[m,j], tn.mj[m,j])
      multitnfnmatvr.mjs[m,j,1:3] <- c(fn.mj[m,j], tn.mj[m,j], matvr.mjt[m,j,1])
      multifntptruenonmatvr.mjs[m,j, 1:3] <- c(fn.mj[m,j], tp.mj[m,j], ytot_vr.mjt[m,j,1] - sum(fn.mj[m,j], tp.mj[m,j]))

      if(sum(!is.na(multiinvrcounts.mjs[m,j,c(fn_index, fp_index, tp_index, tn_index)])) ==4 & sum(multiinvrcounts.mjs[m,j,c(fn_index, fp_index, tp_index, tn_index)]) != ztot_vr.mj[m,j] ){ ztot_vr.mj[m,j] <- sum(multiinvrcounts.mjs[m,j,c(fn_index, fp_index, tp_index, tn_index)])}
      if(sum(!is.na(multioutvrcounts.mjs[m,j,c(fn_index, fp_index, tp_index, tn_index, up_index, un_index)])) ==6 & sum(multioutvrcounts.mjs[m,j,c(fn_index, fp_index, tp_index, tn_index, up_index, un_index)]) != ztot.mj[m,j] ){ ztot.mj[m,j] <- sum(multioutvrcounts.mjs[m,j,c(fn_index, fp_index, tp_index, tn_index, up_index, un_index)])}

    }}



  subfixes <- c("invrsingle", "invrmultiple", "outvrsingle", "outvrmultiple", "tnbinom")
  nindicestruemat.m <- rep(NA, length(subfixes))
  indicestruemat.mx <- array(NA, c(length(subfixes), S))
  for(m in 1:length(subfixes)){
    if(subfixes[m] %in% c("invrsingle", "invrmultiple")){
      nindicestruemat.m[m] <- length(truematinvr_index)
      indicestruemat.mx[m,1:nindicestruemat.m[m]] <- truematinvr_index
    }else{
      nindicestruemat.m[m] <- length(truemat_index)
      indicestruemat.mx[m, 1:nindicestruemat.m[m]] <- truemat_index
    }
  }

  jagsdm <- list()
  jagsdm[["nindicestruemat.m"]] <- nindicestruemat.m
  jagsdm[["indicestruemat.mx"]] <- indicestruemat.mx
  jagsdm[["nindicesmatvr"]] <- length(vrmat_index)
  jagsdm[["nobs.m"]] <- nobs.m
  jagsdm[["ytot_vr.mjt"]] <- ytot_vr.mjt
  jagsdm[["ytot.mjt"]] <- ytot.mjt
  jagsdm[["fn.mj"]] <- fn.mj
  jagsdm[["fp.mj"]] <- fp.mj
  jagsdm[["tn.mj"]] <- tn.mj
  jagsdm[["tp.mj"]] <- tp.mj
  jagsdm[["un.mj"]] <- un.mj
  jagsdm[["up.mj"]] <- up.mj
  jagsdm[["getyears.mjt"]] <- getyears.mjt
  jagsdm[["getyears_ind.mjt"]] <- floor(getyears.mjt) - startyear +1
  jagsdm[["getmidyear.mj"]] <- getmidyear.mj
  jagsdm[["getmidyear.mjt"]] <- getmidyear.mjt
  jagsdm[["nmidyear.mj"]] <- nmidyear.mj
  jagsdm[["matvr.mjt"]] <- matvr.mjt
  jagsdm[["logvrpm.mjt"]] <- log(matvr.mjt/ytot_vr.mjt)
  jagsdm[["ztot.mj"]] <- ztot.mj
  jagsdm[["ztot_vr.mj"]] <- ztot_vr.mj
  jagsdm[["zmatvr.mj"]] <- zmatvr.mj
  jagsdm[["env.mj"]] <- env.mj
  jagsdm[["indicestruemat.jx"]] <- indicestruemat.jx
  jagsdm[["nindicestruemat.j"]] <- nindicestruemat.j
  jagsdm[["truemat.mj"]] <- truemat.mj
  jagsdm[["truematvr.mj"]] <- truematvr.mj
  jagsdm[["truemattot.mj"]] <- truemattot.mj
  jagsdm[["truematprop.mj"]] <- truematprop.mj
  jagsdm[["truenonmat.mj"]] <- ztot_vr.mj - truemat.mj
  jagsdm[["getc.mj"]] <- getc.mj
  jagsdm[["nyears.mj"]] <- nyears.mj
  jagsdm[["tnbinom.dm"]] <- tnbinom.ind
  jagsdm[["fpbinom.dm"]] <- fpbinom.ind
  jagsdm[["overlap.dm"]] <- overlap.ind
  jagsdm[["multioutvr.dm"]] <- multioutvr.ind
  jagsdm[["multiinvr.dm"]] <- multiinvr.ind
  jagsdm[["multitnfnmatvr.dm"]] <- multitnfnmatvr.ind
  jagsdm[["fntruematvrbinom.dm"]] <- fntruematvrbinom.ind

  # jagsdm[["fntruematbinom.dm"]] <- fntruematbinom.ind
  jagsdm[["fntpmultinom.dm"]] <- fntpmultinom.ind
  jagsdm[["truematvrbinom.dm"]] <- truematvrbinom.ind
  jagsdm[["M"]] <- M
  jagsdm[["Tyears"]] <- Tyears
  jagsdm[["multioutvrcounts.mjs"]]<- multioutvrcounts.mjs
  jagsdm[["multiinvrcounts.mjs"]]<- multiinvrcounts.mjs
  jagsdm[["multitnfnmatvr.mjs"]] <- multitnfnmatvr.mjs
  jagsdm[["multifntptruenonmatvr.mjs"]] <- multifntptruenonmatvr.mjs
  jagsdm[["Sinvr"]] <- Sinvr
  jagsdm[["Soutvr"]] <- Soutvr
  jagsdm[["S"]] <- 3
  jagsdm[["varratio"]] <- 0.62 # update LA 2018/12/26
  jagsdm[["fn.tnfnmatvr"]] <- 1
  jagsdm[["tn.tnfnmatvr"]] <- 2
  jagsdm[["matvr.tnfnmatvr"]] <- 3
  jagsdm[["fn.fntptruenonmat"]] <- 1

  jagsdm[["tp.fntptruenonmat"]] <- 2
  jagsdm[["truenonmat.fntptruenonmat"]] <- 3





  #Add specifics for overlapping
  if(length(overlap.ind)>0){
    jagsdm[["invr"]] <- which(B.n == 4)
    jagsdm[["outvr"]] <- which(B.n == 6)
    jagsdm[["inputarray.jzs"]] <- inputarray.nzs
    jagsdm[["Z.j"]] <- Z.n
    jagsdm[["B.j"]] <- B.n
    jagsdm[["iso_alpha_3_code.j"]] <- iso_alpha_3_code.n


    #subset EL to not include larger combis
    include <- which(Z.n>0 )
    jagsdm[["include"]] = include
  }


  #Set reference year for RW
  jagsdm[["tref"]] <- refyear- startyear +1
  jagsdm[["sensmin"]] <- sensmin
  jagsdm[["specmin"]] <- specmin
  jagsdm[["large"]] <- 10000

  jagsout <- c(jags_list_central, jagsdm)


  jagsout$varindicator.ct <- jagsout$icd10.ct+1



  ind = jagsout$tnbinom.dm
  jagsout$multitnfnmatvr.mjs[ind,,]<- cbind(jagsout$tn.mj[ind, ], jagsout$fn.mj[ind, ], jagsout$zmatvr.mj[ind, ])

  ind = jagsout$fpbinom.dm
  J = dim(jagsout$un.mj)[2]
  jagsout$multifptpnonmatvr.mjs <- array(NA, c(jagsout$M, J, 3))
  jagsout$multifptpnonmatvr.mjs[ind,,] <- cbind(jagsout$fp.mj[ind,], jagsout$zmatvr.mj[ind, ] - jagsout$fp.mj[ind,], jagsout$ztot_vr.mj[ind, ] - jagsout$zmatvr.mj[ind, ])

  jagsout$indicesnonmatvr <- c(1,2)


  tref.c <- rep(NA, jagsout$C)


  for(c in 1:jagsout$C){
    c_indices = which(jagsout$getc.i == c)
    start_c = min(jagsout$getstart.i[c_indices])
    end_c = max(jagsout$getend.i[c_indices])
    tref.c[c] <- max(floor((end_c + start_c)/2),2)
  }


  jagsout$tref.c = tref.c


  return(jagsout)


}
