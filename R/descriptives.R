#' @title Get descriptive statistics from a bain model
#' @description Get descriptive statistics from a bain model
#' @param x An object of class 'bain'.
#' @param ci Numeric. Percentage for two-tailed confidence interval, defaults to
#' .95.
#' @return A data.frame.
#' @rdname descriptives
#' @importFrom stats qnorm
#' @export
descriptives <- function(x, ci = .95){
  # Fix this in bain; make sure that it always returns a matrix, even if
  # posterior is only one number
  if(class(x$posterior) == "numeric") x$posterior <- as.matrix(x$posterior)
  data.frame(
    Parameter = names(x$estimates),
    n = x$n,
    Estimate = x$estimates,
    lb = x$estimates + qnorm((1-ci)/2)*sqrt(diag(x$posterior)),
    ub = x$estimates - qnorm((1-ci)/2)*sqrt(diag(x$posterior)),
    row.names = NULL
  )
}
