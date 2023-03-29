adjust_inq <- function(
                       datall,
                       meta,
                       jags_list,
                       fit,
                       z.q) {
  #-----
  # inquiries
  for (j in 1:jags_list$Jinq) {
    ci <-
      tolerance::bintol.int(
        x = jags_list$mat.jinq[j],
        n = jags_list$env.jinq[j],
        side = 2,
        alpha = 0.2,
        method = "CP"
      ) / jags_list$env.jinq[j]
    datall$multiplier[jags_list$isjinq.d][j] <- 1
    datall$pm.adj.postmod[jags_list$isjinq.d][j] <-
      datall$final_pm[jags_list$isjinq.d][j]
    # GG edit NaNs produced by tolerance::bintol.int probably when x = 0, log(0) = -Inf
    if(is.na(ci$`2-sided.lower`) | is.na(ci$`2-sided.upper`)) {
      datall$CI.low.postmod[jags_list$isjinq.d][j] <-
        datall$final_pm[jags_list$isjinq.d][j]
      datall$CI.up.postmod[jags_list$isjinq.d][j] <-
        datall$final_pm[jags_list$isjinq.d][j]
    } else {
      datall$CI.low.postmod[jags_list$isjinq.d][j] <-
        ci[1, "2-sided.lower"]
      datall$CI.up.postmod[jags_list$isjinq.d][j] <-
        ci[1, "2-sided.upper"]
    }
  }
  # inquiries incomplete
  if(!is.null(jags_list$Jinq_incomplete)){
  for (j in 1:jags_list$Jinq_incomplete) {
    datall$multiplier[jags_list$isjinq_incomplete.d][j] <- 1
    datall$pm.adj.postmod[jags_list$isjinq_incomplete.d][j] <-
      datall$final_pm[jags_list$isjinq_incomplete.d][j]
    env <- jags_list$env_incomplete.jinq[j]
    p <- max(0.5 / env, datall$pm.adj.postmod[jags_list$isjinq_incomplete.d][j])
    m <- p * env
    ci <-
      tolerance::negbintol.int(
        x = m,
        n = env - m,
        alpha = 0.2,
        P = 0.99,
        side = 2,
        method = c("CB")
      ) / jags_list$env_incomplete.jinq[j]
    datall$multiplier[jags_list$isjinq_incomplete.d][j] <- 1
    datall$pm.adj.postmod[jags_list$isjinq_incomplete.d][j] <-
      datall$final_pm[jags_list$isjinq_incomplete.d][j]
    # GG edit NaNs produced by tolerance::bintol.int probably when x = 0, log(0) = -Inf
    if(is.na(ci$`2-sided.lower`) | is.na(ci$`2-sided.upper`)) {
      datall$CI.low.postmod[jags_list$isjinq_incomplete.d][j] <-
        datall$final_pm[jags_list$isjinq_incomplete.d][j]
      datall$CI.up.postmod[jags_list$isjinq_incomplete.d][j] <-
        datall$final_pm[jags_list$isjinq_incomplete.d][j]
    } else {
      datall$CI.low.postmod[jags_list$isjinq_incomplete.d][j] <-
        ci[1, "2-sided.lower"]
      datall$CI.up.postmod[jags_list$isjinq_incomplete.d][j] <-
        ci[1, "2-sided.upper"]
    }
  }
}
 return(datall)
} # end function
