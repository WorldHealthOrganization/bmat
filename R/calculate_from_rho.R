



calc_gamma_mat_vr <-
  function(value_column, b_column) {
    list2env(indices_binary_measure(), envir = environment())
    sum(value_column[b_column %in% vrmat_index]) / sum(value_column[b_column %in% vr_index])
  }
calc_gamma_truemat_vr <-
  function(value_column, b_column) {
    list2env(indices_binary_measure(), envir = environment())
    sum(value_column[b_column %in% truematinvr_index]) / sum(value_column[b_column %in% vr_index])
  }
calc_sens  <-
  function(value_column, b_column) {
    list2env(indices_binary_measure(), envir = environment())
    value_column[b_column == tp_index] / sum(value_column[b_column %in% truematinvr_index])
  }
calc_spec  <-
  function(value_column, b_column) {
    list2env(indices_binary_measure(), envir = environment())
    value_column[b_column == tn_index] / sum(value_column[b_column %in% truenonmatinvr_index])
  }
calc_fn_out_truemat  <-
  function(value_column, b_column) {
    list2env(indices_binary_measure(), envir = environment())
    value_column[b_column == fn_index] / sum(value_column[b_column %in% truemat_index])
  }
calc_fn_out_truemat_vr  <-
  function(value_column, b_column) {
    list2env(indices_binary_measure(), envir = environment())
    value_column[b_column == fn_index] / sum(value_column[b_column %in% truematinvr_index])
  }
calc_fp_out_mat_vr  <-
  function(value_column, b_column) {
    list2env(indices_binary_measure(), envir = environment())
    value_column[b_column == fp_index] / sum(value_column[b_column %in% vrmat_index])
  }
calc_rho_truemat_vr <-
  function(value_column, b_column) {
    list2env(indices_binary_measure(), envir = environment())
    sum(value_column[b_column %in% truematinvr_index])
  }
calc_rho_truemat <-
  function(value_column, b_column) {
    list2env(indices_binary_measure(), envir = environment())
    sum(value_column[b_column %in% truemat_index])
  }
calc_rho_tot_vr <-
  function(value_column, b_column) {
    list2env(indices_binary_measure(), envir = environment())
    sum(value_column[b_column %in% vr_index])
  }
calc_rho_nonvr <-
  function(value_column, b_column) {
    list2env(indices_binary_measure(), envir = environment())
    sum(value_column[b_column %in% notvr_index])
  }
