#' @method coef t_test
#' @export
coef.t_test <- function (object, ...)
{
  object$estimate
}

#' @method vcov t_test
#' @export
vcov.t_test <- function (object, ...)
{
  if(length(object$estimate) == 1){
    diag(object$v/object$n, nrow = 1, ncol = 1)
  } else {
    if (grepl("two.sample", tolower(object$method)) & !(grepl("welch", tolower(object$method), fixed = TRUE))) {
      diag(object$v/object$n, nrow = 1, ncol = 1)
    } else {
      df <- sum(object$n) - 2
      v <- 0
      if (object$n[1] > 1)
        v <- v + (object$n[1] - 1) * object$v[1]
      if (object$n[2] > 1)
        v <- v + (object$n[2] - 1) * object$v[2]
      v <- v/df
      diag(v / object$n, nrow = 1, ncol = 1)
    }
}
}
