#' Student's t-test
#'
#' This function is a wrapper for the function \code{\link[stats]{t.test}},
#' which group-specific sample sizes and variances, in addition to the usual
#' output of \code{\link[stats]{t.test}}.
#'
#' This wrapper allows users to enjoy the functionality of bain with the familiar
#' interface of the stats-function t.test.
#'
#' For more documentation, see \code{\link[stats]{t.test}}.
#' @param x An object for which an S3 method of t.test exists (vector or
#' formula).
#' @param \dots arguments passed to \code{\link[stats]{t.test}}.
#' @return A list with class \code{"bain_htest"}.
#' @seealso \code{\link[stats]{t.test}}
#' @keywords htest
#' @examples
#' tmp <- t_test2(extra ~ group, data = sleep)
#' tmp$n
#' tmp$v
#' tmp2 <- t_test(extra ~ group, data = sleep)
#' tmp2$n
#' tmp2$v
#' tmp <- t_test2(extra ~ group, data = sleep, paired = TRUE)
#' tmp$n
#' tmp$v
#' tmp2 <- t_test(extra ~ group, data = sleep, paired = TRUE)
#' tmp2$n
#' tmp2$v
#' t_test2(sesamesim$postnumb)
#' tmp <- t_test2(sesamesim$prenumb)
#' tmp$n
#' tmp$v
#' tmp2 <- t_test(sesamesim$prenumb)
#' tmp2$n
#' tmp2$v
#' tmp <- t_test2(sesamesim$prenumb, sesamesim$postnumb)
#' tmp$n
#' tmp$v
#' tmp2 <- t_test(sesamesim$prenumb, sesamesim$postnumb)
#' tmp2$n
#' tmp2$v
#' tmp <- t_test2(sesamesim$prenumb, sesamesim$postnumb, paired = TRUE)
#' tmp$n
#' tmp$v
#' tmp2 <- t_test(sesamesim$prenumb, sesamesim$postnumb, paired = TRUE)
#' tmp2$n
#' tmp2$v
#' @export
t_test2 <- function(x, ...)
  UseMethod("t_test2")

#' @method t_test2 default
#' @export
t_test2.default <- function(x, ...) {
  cl <- match.call()
  cl[[1]] <- as.name("t.test")
  rval <- eval.parent(cl)
  if (!rval$method == "One Sample t-test") { # If it's a two sample t-test
    if(!hasArg("y")) y <- eval.parent(cl[[3]])
    if (rval$method == "Paired t-test"){
      complete_data <- complete.cases(x, y)
      x <- x[complete_data] - y[complete_data]
      rval$n <- length(x)
      rval$v <- var(x)
    } else { # Independent samples t-test
      y <- y[!is.na(y)]
      x <- x[!is.na(x)]
      rval$n <- c(length(x), length(y))
      rval$v <- c(var(x), var(y))
    }
  } else {
    x <- x[!is.na(x)]
    rval$n <- length(x)
    rval$v <- var(x)
  }

  class(rval) <- c("bain_htest", "htest")
  return(rval)
}


#' @importFrom stats terms
#' @method t_test2 formula
#' @export
t_test2.formula <- function(x, ...) {
  cl <- match.call()
  cl[[1]] <- as.name("t.test")
  names(cl)[match("x", names(cl))] <- "formula"
  rval <- eval.parent(cl)
  data <- as.data.frame(eval.parent(cl[["data"]]))
  model_frame <- stats::model.frame(cl[["formula"]], data)

  if (!rval$method == "One Sample t-test") { # If it's a two sample t-test
    if (rval$method == "Paired t-test"){
      x <- model_frame[model_frame[[2]] == unique(model_frame[[2]])[1], 1] - model_frame[model_frame[[2]] == unique(model_frame[[2]])[2], 1]
      rval$n <- length(x)
      rval$v <- var(x)
    } else { # Independent samples t-test
      rval$n <- c(length(model_frame[model_frame[[2]] == unique(model_frame[[2]])[1], 1]),
                  length(model_frame[model_frame[[2]] == unique(model_frame[[2]])[2], 1]))
      rval$v <- c(var(model_frame[model_frame[[2]] == unique(model_frame[[2]])[1], 1]),
                  var(model_frame[model_frame[[2]] == unique(model_frame[[2]])[2], 1]))
    }
  } else {
    rval$n <- length(model_frame[[1]])
    rval$v <- var(model_frame[[1]])
  }
  class(rval) <- c("bain_htest", "htest")
  return(rval)
}
