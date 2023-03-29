jags_list_central <- function(ssdata, first_year, last_year){

  
  
  yrs <- seq(as.numeric(first_year), as.numeric(last_year))
  nyears <- length(yrs)
  
  iso_alpha_3_code.i <-  startyear.i <- endyear.i  <-  refyear.i <- rep(NA,nrow(ssdata))
  ytot.fori <- ytot_vr.fori <- rhovr.fori <-  c()
  seqyears.fori <-  list()
  
  iso_alpha_3_code.i <- ssdata$iso_alpha_3_code
  for(i in 1:nrow(ssdata)){
    # iso_alpha_3_code.i[i] <- ssdata[i,]$iso_alpha_3_code
    startyear.i[i] <- ssdata[i,]$year_start
    endyear.i[i] <- ssdata[i,]$year_end -1
    refyear.i[i] <- floor((startyear.i[i] + endyear.i[i])/2)
    ytot.fori[i] <-ssdata[i,]$tot_ref #vector of ytots for all obs for all years
    ytot_vr.fori[i] <- ssdata[i,]$tot_vr_ref
    rhovr.fori[i] <- ssdata[i,]$rho_ref
  }
  
  iso_alpha_3_code.c <- unique(iso_alpha_3_code.i)
  C <- length(iso_alpha_3_code.c)
  getc.i <- Getc.i(iso_alpha_3_code.i <- paste(iso_alpha_3_code.i), paste(iso_alpha_3_code.c))
  gett.i <- (refyear.i - first_year + 1)
  getstart.i <- as.numeric(startyear.i) - first_year + 1
  getend.i <- as.numeric(endyear.i) - first_year + 1
  n <- length(iso_alpha_3_code.i)
  
  
  ytot.ct  <- rhovr.ct <- ytot_vr.ct <- array(NA, c(C, nyears))
  for(c in 1:C){
    for(t in 1:nyears){
      i <- which(getc.i == c & gett.i == t)
      if(length(i) == 1) {
        ytot.ct[c,t] <- ytot.fori[i]
        ytot_vr.ct[c,t] <- ytot_vr.fori[i]
        rhovr.ct[c,t] <- rhovr.fori[i]
      }
    }
  }
  
  
  for(c in 1:C){
    x=seq(1,nyears)
    if(sum(!is.na(ytot.ct[c,]))>1){ #if there are more than 1 value for country c do this
      z=ytot.ct[c,]
      interpolateytot <- approxfun(x=x,y=z, method="linear", yright=max(z, na.rm=T), yleft=min(z, na.rm=T))
      ytot.ct[c,] <- interpolateytot(x)
    }else{
      ytot.ct[c,1:nyears] <- ytot.ct[c,which(!is.na(ytot.ct[c,]))] # uses the oen value for all years
    }
    
    
    
    if(sum(!is.na(ytot_vr.ct[c,]))>1){ #if there are more than 1 value for country c do this
      z=ytot_vr.ct[c,]
      interpolateytotvr <- approxfun(x=x,y=z, method="linear", yright=max(z, na.rm=T), yleft=min(z, na.rm=T))
      ytot_vr.ct[c,] <- interpolateytot(x)
    }else{
      ytot_vr.ct[c,1:nyears] <- ytot_vr.ct[c,which(!is.na(ytot_vr.ct[c,]))] # uses the oen value for all years
    }
    
    
    if(sum(!is.na(rhovr.ct[c,]))>1){
      y=rhovr.ct[c,]
      interpolaterho <- approxfun(x=x,y=y, method="linear", yright=max(y, na.rm=T), yleft=min(y, na.rm=T))
      rhovr.ct[c,] <- interpolaterho(x)
    }else{
      rhovr.ct[c,1:nyears] <- rhovr.ct[c,which(!is.na(rhovr.ct[c,]))]
    }
  }
  
  
  #### hard coded model stuff  ####
  B <-  6 # total number of boxes
  #for delta parametrization
  D1 <- diff(diag(nyears), diff=1)
  Dcomb <- t(D1) %*% solve(D1 %*% t(D1))
  K<-2
  Omega0.kk <- Sigma0.kk <- as.matrix(diag(3, K))
  ########################################
  
  jagslistfori<- list("iso_alpha_3_code.c" = iso_alpha_3_code.c,
                      "C" = C, "nyears" = nyears,
                      "B" = B,
                      "Binvr" = 4,
                      "Dcomb" = Dcomb,
                      "nobs" = n,
                      "startyear" = first_year,
                      "endyear" = last_year,
                      "Omega0.kk" = Omega0.kk,
                      "K"=K,
                      "Sigma0.kk" = Sigma0.kk ,
                      "getc.i" = getc.i,
                      "gett.i" = gett.i,
                      "getend.i"  = getend.i,
                      "getstart.i" = getstart.i,
                      "ytot.ct" = ytot.ct,
                      "ytot_vr.ct" = ytot_vr.ct,
                      "rhovr.ct" = rhovr.ct
  )
  
  indices_binary_measure <- indices_binary_measure()
  jagslistfori <- c(jagslistfori, indices_binary_measure)
  
  
  return(jagslistfori)
  
}





