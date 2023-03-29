index_by_data_model <- function(
  main_data
) {
  isjinq_all.d <- (main_data$type=="inq"
                   & !is.na(main_data$final_pm)
                   &!is.na(main_data$final_env)
                   &main_data$include)
  isjinq.d <- isjinq_all.d & main_data$completeness_inq > 0.95
  isjinq_incomplete.d <- isjinq_all.d & !(main_data$completeness_inq > 0.95)
  isj.d<-main_data$type=="vr" & main_data$include
  isjnew.d<- isj.d==FALSE & isjinq.d==FALSE & !is.na(main_data$final_pm) & main_data$include
  return(list(
    isjinq.d = isjinq.d,
    isjinq_incomplete.d = isjinq_incomplete.d,
    isj.d = isj.d,
    isjnew.d = isjnew.d
  ))
}
