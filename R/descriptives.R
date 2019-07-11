#' @method summary bain
#' @importFrom stats qnorm
#' @export
summary.bain <- function(object, ci = .95, ...){
  # Fix this in bain; make sure that it always returns a matrix, even if
  # posterior is only one numberfor
  #if(class(object$posterior) == "numeric") object$posterior <- as.matrix(object$posterior)
  #object$n <- as.vector(object$n)
  N <- object$n
  if (length(object$n) > 1 & object$group_parameters > 1) {
    N <- rep(object$n, each = object$group_parameters)
  }
  if(length(N) != length(object$estimates)){
    N <- c(N, rep(sum(object$n), length(object$estimates)-length(N)))
  }
  data.frame(
    Parameter = names(object$estimates),
    n = N,
    Estimate = object$estimates,
    lb = object$estimates + qnorm((1-ci)/2)*sqrt(diag(object$posterior)),
    ub = object$estimates - qnorm((1-ci)/2)*sqrt(diag(object$posterior)),
    row.names = NULL
  )
}
