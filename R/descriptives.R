#' @method summary bain
#' @importFrom stats qnorm
#' @export
summary.bain <- function(object, ci = .95, ...){
  # Fix this in bain; make sure that it always returns a matrix, even if
  # posterior is only one number
  if(class(object$posterior) == "numeric") object$posterior <- as.matrix(object$posterior)
  if(length(object$n) != length(object$estimates)){
    object$n <- c(object$n, rep(sum(object$n), length(object$estimates)-length(object$n)))
  }
  data.frame(
    Parameter = names(object$estimates),
    n = object$n,
    Estimate = object$estimates,
    lb = object$estimates + qnorm((1-ci)/2)*sqrt(diag(object$posterior)),
    ub = object$estimates - qnorm((1-ci)/2)*sqrt(diag(object$posterior)),
    row.names = NULL
  )
}
