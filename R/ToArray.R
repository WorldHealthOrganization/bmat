#' ToArray
#'
#' Take in list of study data, ie truematvr's and any breakdown information, and for each study element outputs array of unique combinations of boxes
#' that sum to meet aggregated sums using get_unique_combi function. Each study output (study array of combis) is saved into larger array
#' where each index n is unique array of combis for study info n. Z.n is vector of lengths of arrays, ie array n has length z.n[n].
#' Lastly, iso_alpha_3_code.n saved vector of iso_alpha_3_codes from each study.
#'
#' @param studydat
#' @param indices.b
#'
#' @return output list named array_data that holds: combis.nzs, Z.n, B.n, iso_alpha_3_code.n
#' @export
#'
#' @examples
#'
#'
ToArray <- function(studydat) {
   S.dm <- 4
   out.nzs <-  array(NA, c(length(studydat), 40000, S.dm))
   array_data <- list()
   Z.n <- B.n <- iso_alpha_3_code.n <-  rep(NA, length(studydat))

   nobs = length(studydat)

   for (n in 1:nobs) {
      truematvr <- studydat[[n]]$truemat_vr
      truemattot <- studydat[[n]]$truemat
      refyear <-
         floor((studydat[[n]]$year_start + (studydat[[n]]$year_end - 1)) / 2)


      matvr <- sum(studydat[[n]]$ymat_vr)
      ytot_vr <- sum(studydat[[n]]$ytot_vr)
      ytot <- sum(studydat[[n]]$ytot)


      up <- studydat[[n]]$up
      #for CRI we have overlapping with tn and fn included
      tn <- studydat[[n]]$tn
      fn <- studydat[[n]]$fn

      #To determine lower bounds on se and sp
      sample_size <-
         ifelse(!is.na(truematvr), truematvr, truemattot) #determines which are larger studies
      seLB <- 0.1
      #seUB<- ifelse(sample_size >500, 0.9, 1)
      spLB <- 0.97


      array <- get_unique_combis(
         truematvr = truematvr,
         truemattot = truemattot,
         matvr = matvr,
         ytot_vr = ytot_vr,
         ytot = ytot,
         up = up,
         tn = tn,
         fn = fn,

         seLB = seLB,
         spLB = spLB
      )


      # iso_alpha_3_code.n[n] <- studydat[[n]]$iso_alpha_3_code
      Z.n[n] <- dim(array)[1]

      B.n[n] <- dim(array)[2]

      out.nzs[n, 1:Z.n[n], 1:B.n[n]] <-
         as.matrix(array[1:Z.n[n], 1:B.n[n]])


   }#end nobs

   array_data[["combis.nzs"]] <- out.nzs
   array_data[["Z.n"]] <- Z.n
   array_data[["B.n"]] <- B.n
   # GG hack - super complicated way to get isos since the server has some issues with getting them from the loop
   array_data[["iso_alpha_3_code.n"]] <- studydat %>%
     purrr::map_df(tibble::enframe) %>%
     dplyr::filter(
       name %in% c(
         "iso_alpha_3_code"
       )
     ) %>%
     dplyr::mutate(row_id = purrr::pmap(list(1:length(studydat), length(unique(name))), rep) %>% unlist) %>%
     tidyr::spread(name, value) %>%
     dplyr::pull(iso_alpha_3_code) %>%
     unlist()

   #saveRDS(array_data, paste(filepathinputs, "array_data.RDS", sep=""))

   return(array_data)


}#end function









get_unique_combis <- function(truematvr,
                              truemattot,
                              matvr,
                              ytot_vr,
                              ytot,
                              up,
                              tn,
                              fn,
                              seLB,
                              spLB) {
   indices_binary_measure <- indices_binary_measure()
   list2env(indices_binary_measure, envir = environment())
   invr <- ifelse(!is.na(truematvr) | up == 0 & !is.na(up), T, F)
   hasbreakdown <- ifelse(!is.na(tn) & !is.na(fn), T, F)

   if (invr) {
      cats <- c("tn", "fn", "fp", "tp")
      tp_vector <- seq(0, min(matvr, truematvr))
      all.combis <-
         data.frame(array(NA, c(length(tp_vector), length(cats))))
      names(all.combis) <- cats

      all.combis[, tp_index] <- tp_vector
      all.combis[, fp_index] <- matvr - all.combis[, tp_index]
      all.combis[, fn_index] <- truematvr - all.combis[, tp_index]
      all.combis[, tn_index] <- ytot_vr - matvr - all.combis[, fn_index]

      #filter tn values based on lower bound for sp here

      include.combis <- dplyr::filter(
         all.combis,
         tp >= qbinom(
            p = 0.025,
            prob = seLB ,
            size  = truematvr
         ) &
            #all.combis[,tp_index] <= qbinom(p=0.975, prob = seUB, size  = truematvr) &
            tn >=  qbinom(
               p = 0.025,
               prob = spLB,
               size = ytot_vr - truematvr
            ) &
            fp >= 0 &
            tn >=  qbinom(
               p = 0.025,
               prob = spLB,
               size = ytot_vr - truematvr
            ) &
            fn >= 0
      )
      array <- include.combis


   } else{
      cats <- c("tn", "fn", "fp", "tp", "up", "un")
      if (!hasbreakdown) {
         tp_vector <- seq(0, min(matvr, truemattot))
         fn_vector <- seq(0 , min(ytot_vr - matvr, truemattot))
         tpfn_vector <- expand.grid(fn_vector, tp_vector)
         names(tpfn_vector) <- c("fn", "tp")
         all.combis <-
            data.frame(array(NA, c(
               dim(tpfn_vector)[1], length(cats)
            )))
         names(all.combis) <- cats
         all.combis[, tp_index] <- tpfn_vector$tp
         all.combis[, fn_index] <- tpfn_vector$fn
         all.combis[, fp_index] <- matvr - all.combis[, tp_index]
         all.combis[, tn_index] <- ytot_vr - matvr - all.combis[, fn_index]
         all.combis[, up_index] <-
            truemattot - all.combis[, tp_index] - all.combis[, fn_index]
         all.combis[, un_index] <- ytot - (all.combis[, tp_index] +
                                        all.combis[, fn_index] +
                                        all.combis[, fp_index] +
                                        all.combis[, tn_index] +
                                        all.combis[, up_index])

      } else{
         tp_vector <- seq(0, min(matvr, truemattot - fn))
         all.combis <-
            data.frame(array(NA, c(
               length(tp_vector)[1], length(cats)
            )))
         names(all.combis) <- cats
         all.combis[, tp_index] <- tp_vector
         all.combis[, fn_index] <- fn
         all.combis[, fp_index] <- matvr - all.combis[, tp_index]
         all.combis[, tn_index] <- tn
         all.combis[, up_index] <-
            truemattot - all.combis[, tp_index] - all.combis[, fn_index]
         all.combis[, un_index] <- ytot - (all.combis[, tp_index] +
                                        all.combis[, fn_index] +
                                        all.combis[, fp_index] +
                                        all.combis[, tn_index] +
                                        all.combis[, up_index])


      }







      truematvr <- (all.combis[, tp_index] + all.combis[, fn_index])
      truenonmatvr <- (all.combis[, tn_index] + all.combis[, fp_index])
      truepmin <- (all.combis[, tp_index] + all.combis[, fn_index]) / ytot_vr
      missed <- (all.combis[, up_index] + all.combis[, un_index])

      minpm = 0.5 * min(truepmin)
      pmmax = 2 * max(truepmin)


      include.combis <- dplyr::filter(
         all.combis,
        tp >= qbinom(
            p = 0.025,
            prob = seLB ,
            size  = truematvr
         ) &
            tn >=  qbinom(
               p = 0.025,
               prob = spLB,
               size = ytot_vr - truematvr
            ) &
            fp >= 0 &
            tn >=  qbinom(
               p = 0.025,
               prob = spLB,
               size = ytot_vr - truematvr
            ) &
           fn >= 0 &
            up >= qbinom(
               p = 0.025,
               prob = 0.5 * truepmin,
               size  = missed
            ) &
            up <= qbinom(
               p = 0.975,
               prob = 2 * truepmin,
               size  = missed
            ) &
            un >= 0 &
            missed > 0

      )



      include.combis_invronly <- include.combis[, -c(up_index, un_index)]
      index <- which(duplicated(include.combis_invronly))
      if (length(index) > 0) {
         include.combis_invronly <- include.combis_invronly[-index, ]
      }
      array <- include.combis_invronly

   }#end overlap out VR

   return(array)
}
