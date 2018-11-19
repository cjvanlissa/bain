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
  data.frame(
    Parameter = names(x$estimates),
    n = ifelse(length(x$n) == length(x$estimates), x$n, rep(x$n, length(x$estimates))),
    Estimate = x$estimates,
    lb = x$estimates + qnorm((1-.95)/2)*sqrt(diag(x$posterior)),
    ub = x$estimates - qnorm((1-.95)/2)*sqrt(diag(x$posterior)),
    row.names = NULL
  )
}
