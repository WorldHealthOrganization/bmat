indices_binary_measure <- function() {
  
  # use first 4 boxes for inside VR only
  tn_index <- 1
  fn_index <- 2
  fp_index <- 3
  tp_index <- 4
  up_index <- 5
  un_index <- 6
  
  tot_index = c(tn_index,fn_index,fp_index,tp_index,up_index,un_index)
  vr_index = c(tn_index,fn_index,fp_index,tp_index)
  notvr_index = setdiff(tot_index, vr_index)
  vrmat_index = c(fp_index, tp_index)
  notvrmat_index = setdiff(vr_index, vrmat_index)
  allbutun_index = setdiff(tot_index, un_index)
  truematinvr_index = c(tp_index,fn_index)
  truenonmatinvr_index =  c(tn_index,fp_index)
  truemat_index = c(truematinvr_index,up_index)
  matvr_index = c(fp_index, tp_index)
  
  indices_binary_measure <- list(matvr_index = matvr_index,
                                 tot_index = tot_index,
                                 vr_index = vr_index,
                                 notvr_index = notvr_index,
                                 vrmat_index = vrmat_index,
                                 notvrmat_index = notvrmat_index,
                                 allbutun_index = allbutun_index,
                                 truematinvr_index = truematinvr_index,
                                 truenonmatinvr_index =  truenonmatinvr_index,
                                 truemat_index = truemat_index,
                                 tn_index = tn_index,
                                 fn_index = fn_index,
                                 fp_index = fp_index,
                                 tp_index = tp_index,
                                 up_index = up_index,
                                 un_index = un_index)
  
  
  
  return(indices_binary_measure)
}