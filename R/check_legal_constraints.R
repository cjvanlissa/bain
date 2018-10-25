check_legal_constraints_cj <- function(hyp_mat, n_constraints, numAR){
  hyp_mat_and_neg <- rbind(hyp_mat, -1*hyp_mat)

  same_constraints <- duplicated(hyp_mat_and_neg[, -ncol(hyp_mat_and_neg)])
  same_constant <- duplicated(hyp_mat_and_neg)
  if(!all(same_constraints == same_constant)) error <- 2
}
