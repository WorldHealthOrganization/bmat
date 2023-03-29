
process_vr_data<- function(# input files:
  vr_data,
  meta_precrisis,
  icd10codes = c("103", "104", "10M", "101")) {

  exclude <- as.numeric(vr_data$Frmat) == 9 |
    as.numeric(vr_data$Year) < meta_precrisis$year.t[1] |
    !is.element(vr_data$Country, meta_precrisis$whocode.c)
  #mean(exclude)
  vrdat <- vr_data[!exclude,]
  
  year.j <- as.numeric(vrdat$Year)
  #table(vrdat$List)
  cause.j <- vrdat$Cause
  #table(cause.j)
  #vrdat$tot
  
  # save gr2/all and gr2/(all-hiv)
  
  # ages: sum up deaths between ages 15 and 49
  # note that for format==7 and 8, the age group is 45-54
  columnstosum <- which(is.element(names(vrdat), paste("A", seq(15,45,5), sep = "")))
  totals.j <- apply(vrdat[, columnstosum],1, sum) # this includes various causes
  names(totals.j) <- cause.j
  # add (distribute) deaths with unknown ages
  unknown.j <- vrdat$unk
  unknown.j[is.na(unknown.j)] <- 0
  # for maternal deaths, add all in unknown ages to the total maternal deaths
  maternalset <- c("O96", "O97", "mat")
  
  # for total deaths, redistribute according to age pattern of deaths
  # so add unknown*prop of deaths with known ages in 15-49 age group
  # resdistribute to adolescents first (before total.j is updated)
  # for adolescents: total deaths same idea, for mat deaths, redistribute according to prop ado/mat total
  
  totals.j <- ifelse(is.element(cause.j, maternalset),
                     totals.j + unknown.j,
                     #                     totals.j + unknown.j*totals.j/vrdat$tot)
                     # update 2018/11/9
                     totals.j)# + unknown.j*totals.j/vrdat$tot)
  
  iso.j <- vrdat$Country
  # next step: combine all rows referring to one country-year into one combined row
  # start with empty vectors to store this info
  late.k <- iso.k <- year.k <- tot.k <- matincllate.k <- ill.k <- icd.k <-
    #  totado.k <- matincllateado.k <- lateado.k <-
    grp2.k <-
    NULL
  # we don't pay attention to ill in adolescents yet
  for (iso in unique(iso.j)){
    for (year in unique(year.j[iso.j==iso])){
      select <- iso.j==iso & year.j==year
      if (sum(select & cause.j == "all")==1) {
        # we only include country-year with 1 VR entry for all deaths
        iso.k <- c(iso.k, iso)
        year.k <- c(year.k, year)
        tot.k <- c(tot.k, totals.j[select & cause.j == "all"])
        grp2.k <- c(grp2.k, ifelse(sum(select & cause.j == "gr2") == 1,
                                   totals.j[select & cause.j == "gr2"], 0))
        #  totado.k <- c(totado.k, totalsado.j[select & cause.j == "all"])
        # if info on ill or mat is missing, we assume that these entries are 0
        ill.k <- c(ill.k, ifelse(sum(select & cause.j == "ill") ==1,
                                 totals.j[select & cause.j == "ill"],0))
        # mat refers to mat including late for icd10 as well
        matincllate.k <- c(matincllate.k, ifelse(sum(select & cause.j == "mat") ==1,
                                                 totals.j[select & cause.j == "mat"],0))
        #    matincllateado.k <- c(matincllateado.k, ifelse(sum(select & cause.j == "mat") ==1,
        #                                                  totalsado.j[select & cause.j == "mat"],0))
        icd <- vrdat$List[select][1]
        icd.k <- c(icd.k, paste(icd))
        if (!is.element(icd, icd10codes)){
          late.k <- c(late.k,NA)
          #       lateado.k <- c(lateado.k,NA)
        } else {
          # 096 and O97 are late maternal (could be missing?)
          indices <- which(select & is.element(cause.j , c("O96", "O97")))
          late.k <- c(late.k, ifelse(length(indices)>0, sum(totals.j[indices]),0))
          #        lateado.k <- c(lateado.k, ifelse(length(indices)>0, sum(totalsado.j[indices]),0))
        }
      }
    }
  }
  ill.k[is.na(ill.k)] <- 0
  matincllate.k[is.na(matincllate.k)] <- 0
  #matincllateado.k[is.na(matincllateado.k)] <- 0
  # same for late if it's icd10 code
  icd10.k <- ifelse(is.element(icd.k, icd10codes), TRUE, FALSE)
  late.k[icd10.k & is.na(late.k)] <- 0 #update from round 2015 (but there were none in 2015 round)
  #lateado.k[icd10.k & is.na(lateado.k)] <- 0 #update from round 2015 (but there were none in 2015 round)
  
  # some country-specific exclusions
  include.k <- rep(TRUE, length(iso.k))
  excludereason.k <- rep(NA, length(iso.k))
  #tmp <- rep(NA, length(iso.k))
  #tmp[select] <- "bla"
  #table(excludereason.k)
  
  getc.k <- Getc.i(iso.c = meta_precrisis$whocode.c, iso.i=iso.k)
  
  
  vrdat2 <- data.frame(name = meta_precrisis$name.c[getc.k],
                       whoiso = iso.k,
                       iso = meta_precrisis$iso.c[getc.k],
                       year = year.k+0.5,#start = year.k, end = year.k+1,
                       start = year.k, end = year.k+1,
                       mat.incl.late = matincllate.k,
                       mat.late = late.k,
                       env = tot.k,
                       #                       mat.incl.late.ado = matincllateado.k,
                       #                       mat.late.ado = lateado.k,
                       #                       env.ado = totado.k,
                       propill = ill.k/tot.k,
                       propgrp2 = grp2.k/tot.k,
                       ill = ill.k,
                       include = include.k,
                       exclude.reason=excludereason.k,
                       isicd10 = icd10.k, icd = icd.k
                       
  )
  remove(vrdat)
  vrdat2$mat <- ifelse(!is.na(vrdat2$mat.late), vrdat2$mat.incl.late - vrdat2$mat.late, vrdat2$mat.incl.late)
  
  return(vrdat2)
}
