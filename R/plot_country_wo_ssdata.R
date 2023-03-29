

plot_country_nossdata <- function(sesp_countrynodatallyears_samples,jags_list){
  
  sens.st <- sesp_countrynodatallyears_samples["se",,]
  spec.st <- sesp_countrynodatallyears_samples["sp",,]
  
  setrans.st <- sesp_countrynodatallyears_samples["se_trans",,]
  sptrans.st <- sesp_countrynodatallyears_samples["sp_trans",,]
  
  vars = c("t", "sens", "lowersens", "uppersens", "spec", "lowerspec", "upperspec")
  blankcountry <- data.frame(array(NA, c(dim(sens.st)[2], length(vars))))
  
  names(blankcountry) <- vars
  
  lower_sens.t <- upper_sens.t <- lower_spec.t <- upper_spec.t <- sens.t <- spec.t <- rep(NA, jags_list$nyears)
  lower_senstr.t <- upper_senstr.t <- lower_spectr.t <- upper_spectr.t <- senstr.t <- spectr.t <- rep(NA, jags_list$nyears)
  for(t in 1:jags_list$nyears){
    lower_sens.t[t] <- quantile(sens.st[,t], probs = c(0.1))
    upper_sens.t[t] <- quantile(sens.st[,t], probs = c(0.9))
    sens.t[t] <- quantile(sens.st[,t], probs = c(0.5))
    lower_spec.t[t] <- quantile(spec.st[,t], probs = c(0.1))
    upper_spec.t[t] <- quantile(spec.st[,t], probs = c(0.9))
    spec.t[t] <- quantile(spec.st[,t], probs = c(0.5))
    
    lower_senstr.t[t] <- quantile(setrans.st[,t], probs = c(0.1))
    upper_senstr.t[t] <- quantile(setrans.st[,t], probs = c(0.9))
    senstr.t[t] <- quantile(setrans.st[,t], probs = c(0.5))
    lower_spectr.t[t] <- quantile(sptrans.st[,t], probs = c(0.1))
    upper_spectr.t[t] <- quantile(sptrans.st[,t], probs = c(0.9))
    spectr.t[t] <- quantile(sptrans.st[,t], probs = c(0.5))
    
    
  }
  blankcountry$t <- seq(1, length(sens.t))
  blankcountry$sens <- sens.t
  blankcountry$lowersens <- lower_sens.t
  blankcountry$uppersens <- upper_sens.t
  blankcountry$spec <- spec.t
  blankcountry$lowerspec <- lower_spec.t
  blankcountry$upperspec <- upper_spec.t
  
  
  
  # blankcountry$setr <- senstr.t
  # blankcountry$lowersetr <- lower_senstr.t
  # blankcountry$uppersetr <- upper_senstr.t
  # blankcountry$sptr <- spectr.t
  # blankcountry$lowersptr <- lower_spectr.t
  # blankcountry$uppersptr <- upper_spectr.t
  
  
  pdf(paste(output.dir, "tempfig/plot_countrynodata.pdf", sep=""))
  par(mfrow = c(2,1))
  miny = min(blankcountry$sens, blankcountry$lowersens, blankcountry$uppersens)
  maxy = max(blankcountry$sens, blankcountry$lowersens, blankcountry$uppersens)
  plot(blankcountry$t, blankcountry$sens, ylim = c(miny, maxy),type="l", col="darkblue", lwd = 3, ylab = "sens", xaxt="n", xlab = "Year", main= "Estimates of sens for a country with no data")
  AddCIs(CI.low.t = blankcountry$lowersens, CI.up.t = blankcountry$uppersens, seq.years.t = blankcountry$t, col= t_col(col = "darkblue", percent = 70) )
  axis(side = 1, labels = seq(1985, 2017), at = seq(1,33))
  #abline(v = 26, col = "red", lwd = 3)
  
  
  miny = min(blankcountry$spec, blankcountry$lowerspec, blankcountry$upperspec)
  maxy =  max(blankcountry$spec, blankcountry$lowerspec, blankcountry$upperspec)
  plot(blankcountry$t, blankcountry$spec, ylim = c(miny, maxy),type="l", col="darkblue", lwd = 3, ylab = "spec", xaxt="n", xlab = "Year", main= "Estimates of spec for a country with no data")
  AddCIs(CI.low.t = blankcountry$lowerspec, CI.up.t = blankcountry$upperspec, seq.years.t = blankcountry$t, col=t_col(col = "darkblue", percent = 70) )
  axis(side = 1, labels = seq(1985, 2017), at = seq(1,33))

  dev.off()
}
  


