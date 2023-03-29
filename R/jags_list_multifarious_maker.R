jags_list_multifarious_maker <- function(main_data_subset,
                        jags_list_central,
                        subfix,
                        index_of_dm
){

  indices_binary_measure <- indices_binary_measure()
  list2env(indices_binary_measure, envir = environment())
  b = 4
  B = jags_list_central$B
  m = 3
  nyears = jags_list_central$nyears
  startyear = jags_list_central$startyear
  endyear = jags_list_central$endyear
  getc.i = jags_list_central$getc.i
  gett.i = jags_list_central$gett.i

  getc.j <-  getc.i[index_of_dm] #Get country indices for j studies
  gett.j <- gett.i[index_of_dm]  #Get year indices for j studies
  nobs <- length(main_data_subset)
  addjagsdata <- list()



  nyears.j <- fn.j <- fp.j <- tn.j <- tp.j <- up.j <- un.j <- ztot.j <- zmatvr.j <-  ztot_vr.j <- truemat.j <- truematvr.j <- truemattot.j <- truenonmat.j <- refyears.j <- rep(NA, nobs)
  matvr.jt <- ytot.jt <- ytot_vr.jt <- getyearindices.jt <- array(NA, c(nobs, nyears))
  multicounts_invr.jts <- array(NA, c(nobs, nyears, b))
  multicounts_outvr.jts <- array(NA, c(nobs, nyears, B))
  multicounts_tnfnmatvr.jts <- array(NA, c(nobs, nyears, m))
  multicounts_fntptruenonmatvr.jts <- array(NA, c(nobs, nyears, m))

  for(j in 1:nobs){
    nyears.j[j] <-  ifelse(length(seq( main_data_subset[[j]]$year_start , main_data_subset[[j]]$year_end-1 )) == length(main_data_subset[[j]]$ymat_vr),
                           length(seq( main_data_subset[[j]]$year_start , main_data_subset[[j]]$year_end-1 )), length(main_data_subset[[j]]$ymat_vr))
    refyears.j[j] <- floor( (main_data_subset[[j]]$year_start +  (main_data_subset[[j]]$year_end-1))/2)
    midyear_index = which(main_data_subset[[j]]$year_mid == refyears.j[j])
    matvr.jt[j, 1:nyears.j[j]] <- main_data_subset[[j]]$ymat_vr
    ytot.jt[j, 1:nyears.j[j]] <- main_data_subset[[j]]$ytot
    ytot_vr.jt[j, 1:nyears.j[j]] <- main_data_subset[[j]]$ytot_vr
    ztot.j[j] <- sum(main_data_subset[[j]]$ytot)
    ztot_vr.j[j] <- sum(main_data_subset[[j]]$ytot_vr)
    zmatvr.j[j] <- sum( main_data_subset[[j]]$ymat_vr)

    if(length(seq( main_data_subset[[j]]$year_start , main_data_subset[[j]]$year_end-1 )) == length(main_data_subset[[j]]$ymat_vr)){
      getyearindices.jt[j,1:nyears.j[j]] <- seq( main_data_subset[[j]]$year_start , main_data_subset[[j]]$year_end-1 )

    }else{

      getyearindices.jt[j,1:nyears.j[j]]<- seq(main_data_subset[[j]]$year_start, main_data_subset[[j]]$year_start +  nyears.j[j]-1)

    }


    truemat.j[j] <- ifelse(!is.na(main_data_subset[[j]]$truemat_vr), main_data_subset[[j]]$truemat_vr, main_data_subset[[j]]$truemat)
    truematvr.j[j] <- main_data_subset[[j]]$truemat_vr
    truemattot.j[j] <- main_data_subset[[j]]$truemat
    if(subfix %in% c("truematvrbinom", "fntruematvrbinom", "multiinvr", "inVR1yr", "inVRyrs")){
      truemat.j[j] <- main_data_subset[[j]]$truemat_vr
    }
    truenonmat.j[j] <- sum(ytot_vr.jt[j,], na.rm=T) - truemat.j[j]


    fn.j[j] <- main_data_subset[[j]]$fn
    fp.j[j] <- main_data_subset[[j]]$fp
    tn.j[j] <- main_data_subset[[j]]$tn
    tp.j[j] <- main_data_subset[[j]]$tp
    up.j[j] <- main_data_subset[[j]]$up
    un.j[j] <- main_data_subset[[j]]$un



    multicounts_invr.jts[j,1:nyears.j[j], c(fn_index, fp_index, tn_index, tp_index)] <- c(fn.j[j], fp.j[j], tn.j[j], tp.j[j])
    multicounts_outvr.jts[j,1:nyears.j[j], c(fn_index, fp_index, tn_index, tp_index, up_index, un_index)] <- c(fn.j[j], fp.j[j], tn.j[j], tp.j[j], up.j[j], un.j[j])
    multicounts_tnfnmatvr.jts[j,1:nyears.j[j], 1:m] <- c(tn.j[j], fn.j[j], zmatvr.j[j])
    multicounts_fntptruenonmatvr.jts[j,1:nyears.j[j], 1:m] <- c(fn.j[j], tp.j[j], ztot_vr.j[j] - truenonmat.j[j])


  }#end j loop





  if(subfix == "overlap"){ ###  Overlapping data models
    array_data <- ToArray(main_data_subset) #treated all studies like within vr because too many combis.
    combis.nzs <- array_data[["combis.nzs"]]

    Z.n <- array_data[["Z.n"]]
    B.n <- array_data[["B.n"]]
    iso_alpha_3_code.n <- array_data[["iso_alpha_3_code.n"]]
    addjagsdata[["Z.n"]] <- Z.n
    addjagsdata[["B.n"]] <- B.n
    addjagsdata[["iso_alpha_3_code.n"]] <- iso_alpha_3_code.n
    addjagsdata[[paste("inputarray.nzs", sep="")]] = combis.nzs
    #This is a multinomial array where the columns related to the indices.b, excluding up and un.
    combis.nzs[1,1:Z.n[1],]
  }



  #Save everything in addjagsdata
  addjagsdata[["nobs"]] <- nobs
  addjagsdata[["getc.j"]] = getc.j
  addjagsdata[["gett.j"]] = gett.j
  addjagsdata[["nyears.j"]] = nyears.j
  addjagsdata[["ztot.j"]] = ztot.j
  addjagsdata[["ztot_vr.j"]] = ztot_vr.j
  addjagsdata[["zmatvr.j"]] = zmatvr.j
  addjagsdata[["truemat.j"]] = truemat.j
  addjagsdata[["truenonmat.j"]] = truenonmat.j
  addjagsdata[["truematvr.j"]] = truematvr.j
  addjagsdata[["truemattot.j"]] = truemattot.j
  addjagsdata[["refyears.j"]] = refyears.j
  addjagsdata[["matvr.jt"]] = matvr.jt
  addjagsdata[["ytot.jt"]] = ytot.jt
  addjagsdata[["ytot_vr.jt"]] = ytot_vr.jt
  addjagsdata[["nonmatvr.jt"]] = ytot_vr.jt - matvr.jt
  addjagsdata[["getyearindices.jt"]] = getyearindices.jt
  addjagsdata[["b"]] <- b

  addjagsdata[["fn.j"]] = fn.j
  addjagsdata[["fp.j"]] = fp.j
  addjagsdata[["tp.j"]] = tp.j
  addjagsdata[["tn.j"]] = tn.j
  addjagsdata[["up.j"]] = up.j
  addjagsdata[["un.j"]] = un.j
  addjagsdata[["multinom_invr.jts"]] <- multicounts_invr.jts
  addjagsdata[["multinom_outvr.jts"]] <- multicounts_outvr.jts



  return(addjagsdata)
}
