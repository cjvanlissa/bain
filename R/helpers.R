deprecated_arguments <- function(args, call = NULL){
  if(is.null(call)){
    call <- sys.call(-1)
  }
  call <- as.list(call)
  call_names <- names(call)[-1]
  function_name <- deparse(call[[1]])
  if (any(call_names %in% names(args))) {
    warning(
      "You are using a deprecated argument when calling '", function_name, "'. Check the documentation. Deprecated arguments are:\n  ",
      paste(paste0(names(args)[which(names(args) %in% call_names)], ": ", args[which(names(args) %in% call_names)]),
            collapse = "\n  ")
      , call. = FALSE)
  }
}


checkcov <- function(Sigma) {
  error = 0
  if (!isTRUE(all.equal(as.matrix(Sigma), t(Sigma), tolerance = 1e-10))) {
    error = 1
  } else{
    if (any(eigen(Sigma)$values <= 0)) {
      error = 1
    }
  }
  return(error)
}


covmatrixfun <- function(inv_cov_list, grouppara, jointpara, P) {
  inv_upperleft <-
    lapply(inv_cov_list, function(x)
      x[1:grouppara, 1:grouppara])
  if (jointpara > 0) {
    inv_upperright <-
      lapply(inv_cov_list, function(x)
        matrix(x[1:grouppara, (grouppara + 1):(grouppara + jointpara)], grouppara, jointpara))
    inv_lowerleft <-
      lapply(inv_cov_list, function(x)
        matrix(x[(grouppara + 1):(grouppara + jointpara), 1:grouppara], jointpara, grouppara))
    inv_lowerright <-
      lapply(inv_cov_list, function(x)
        matrix(x[(grouppara + 1):(grouppara + jointpara), (grouppara + 1):(grouppara +
                                                                             jointpara)], jointpara, jointpara))
    inv_lowerright_matrix <- diag(0, jointpara)
  }

  inv_cov_total <- diag(0, P * grouppara)

  #or using cbind and rbind
  for (p in 1:P) {
    inv_cov_total[((p - 1) * grouppara + 1):(p * grouppara), ((p - 1) * grouppara +
                                                                1):(p * grouppara)] <- inv_upperleft[[p]]
    if (jointpara > 0) {
      inv_lowerright_matrix <- inv_lowerright_matrix + inv_lowerright[[p]]
    }
  }

  if (jointpara > 0) {
    inv_cov_total <- cbind(inv_cov_total, do.call(rbind, inv_upperright))
    inv_cov_total <-
      rbind(inv_cov_total, cbind(do.call(cbind, inv_lowerleft), inv_lowerright_matrix))
  }

  covmatrix <- solve(inv_cov_total)
  return(covmatrix)
}


add_col<-function(x,n_cov){
  if(length(x)!=0) x<-cbind(x, matrix(0,nrow(x),n_cov))
  return(x)
}
