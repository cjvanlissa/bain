#' Student's t-test
#'
#' This function is a wrapper for the function \code{\link[stats]{t.test}},
#' which returns group-specific sample sizes and variances, in addition to the
#' usual output of \code{\link[stats]{t.test}}.
#'
#' This wrapper allows users to enjoy the functionality of bain with the
#' familiar interface of the stats-function t.test.
#'
#' For more documentation, see \code{\link[stats]{t.test}}.
#' @param x An object for which an S3 method of t.test exists (vector or
#' formula).
#' @param \dots arguments passed to \code{\link[stats]{t.test}}.
#' @return A list with class \code{"t_test"} containing the following
#' components: \item{statistic}{the value of the t-statistic.}
#' \item{parameter}{the degrees of freedom for the t-statistic.}
#' \item{p.value}{the p-value for the test.} \item{conf.int}{a confidence
#' interval for the mean appropriate to the specified alternative hypothesis.}
#' \item{estimate}{the estimated mean or difference in means depending on
#' whether it was a one-sample test or a two-sample test.}
#' \item{null.value}{the specified hypothesized value of the mean or mean
#' difference depending on whether it was a one-sample test or a two-sample
#' test.} \item{alternative}{a character string describing the alternative
#' hypothesis.} \item{method}{a character string indicating what type of t-test
#' was performed.} \item{data.name}{a character string giving the name(s) of
#' the data.}\item{v}{The variance or group-specific variances.}\item{n}{The
#' sample size, or group-specific sample size.}
#' @seealso \code{\link[stats]{t.test}}
#' @keywords htest
#' @examples
#' tmp <- t_test(extra ~ group, data = sleep)
#' tmp$n
#' tmp$v
#' tmp2 <- t_test(extra ~ group, data = sleep)
#' tmp2$n
#' tmp2$v
#' tmp <- t_test(extra ~ group, data = sleep, paired = TRUE)
#' tmp$n
#' tmp$v
#' tmp2 <- t_test(extra ~ group, data = sleep, paired = TRUE)
#' tmp2$n
#' tmp2$v
#' t_test(sesamesim$postnumb)
#' tmp <- t_test(sesamesim$prenumb)
#' tmp$n
#' tmp$v
#' tmp2 <- t_test(sesamesim$prenumb)
#' tmp2$n
#' tmp2$v
#' tmp <- t_test(sesamesim$prenumb, sesamesim$postnumb)
#' tmp$n
#' tmp$v
#' tmp2 <- t_test(sesamesim$prenumb, sesamesim$postnumb)
#' tmp2$n
#' tmp2$v
#' tmp <- t_test(sesamesim$prenumb, sesamesim$postnumb, paired = TRUE)
#' tmp$n
#' tmp$v
#' tmp2 <- t_test(sesamesim$prenumb, sesamesim$postnumb, paired = TRUE)
#' tmp2$n
#' tmp2$v
#' @export
t_test <- function(x, ...)
  UseMethod("t_test")

#' @method t_test default
#' @export
t_test.default <- function(x, ...) {
  cl <- match.call()
  cl[[1]] <- as.name("t.test")
  var_eq <- FALSE
  if("var.equal" %in% names(cl)){
    var_eq <- cl[["var.equal"]]
  } else {
    if(length(cl) > 6){
      if(names(cl)[7] == "") {
        var_eq <- cl[["var.equal"]]
      }
    }
  }
  rval <- eval.parent(cl)
  if (!rval$method == "One Sample t-test") { # If it's a two sample t-test
    if("y" %in% names(cl)){
      y <- eval.parent(cl[["y"]])
    } else {
      if(!is.null(cl[[3]])){
        if(names(cl)[3] == ""){
          y <- eval.parent(cl[[3]])
        }
      }
    }
    #if(!hasArg("y")) y <- eval.parent(cl[[3]])
    if (rval$method == "Paired t-test"){
      complete_data <- complete.cases(x, y)
      x <- x[complete_data] - y[complete_data]
      rval$n <- length(x)
      rval$v <- var(x)
    } else { # Independent samples t-test
      y <- y[!is.na(y)]
      x <- x[!is.na(x)]
      rval$n <- c(length(x), length(y))
      #browser()
      if (var_eq) {
        df <- length(x) + length(y) - 2
        v <- 0
        if (length(x) > 1)
          v <- v + (length(x) - 1) * var(x)
        if (length(y) > 1)
          v <- v + (length(y) - 1) * var(y)
        v <- v/df
        rval$v <- v
        rval$v <- c(var(x), var(y))
      } else {
        rval$v <- c(var(x), var(y))
      }
    }
  } else {
    x <- x[!is.na(x)]
    rval$n <- length(x)
    rval$v <- var(x)
  }

# BELOW HERBERT HAS RELABELLED THE ESTIMATES. THIS AVOIDS POSSIBLE
# FUTURE PROBLEMS LIKE THE ONE WE ENCOUNTERED WITH "DIFFERENCE".
# ALSO IN PRINCIPLE
# THE PARSER CANNOT PROVIDE NAMES WITH SPACES, SO THIS IS SAFER.

  if (rval$method == "One Sample t-test"){names(rval$estimate) <- "x"}
  else if (rval$method == "Paired t-test"){names(rval$estimate) <- "difference"}
  else {names(rval$estimate) <- c("x","y")}
  class(rval) <- c("t_test", "htest")
  return(rval)
}


#' @importFrom stats terms
#' @method t_test formula
#' @export
t_test.formula <- function(x, ...) {
  cl <- match.call()
  cl[[1]] <- as.name("t.test")
  names(cl)[match("x", names(cl))] <- "formula"
  rval <- eval.parent(cl)
  data <- as.data.frame(eval.parent(cl[["data"]]))
  model_frame <- stats::model.frame(cl[["formula"]], data)
  response <- attr(attr(model_frame, "terms"), "response")
  g <- factor(model_frame[[-response]])
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
  if (length(rval$estimate) == 2L)

# BELOW HERBERT HAS REMOVED "mean of" FROM THE LABELLING. THIS AVOIDS POSSIBLE
# FUTURE PROBLEMS LIKE THE ONE WE ENCOUNTERED WITH "DIFFERENCE". ALSO IN PRINCIPLE
# THE PARSER CANNOT PROVIDE NAMES WITH SPACES, SO THIS IS SAFER.
    names(rval$estimate) <- paste0("group", levels(g))
  class(rval) <- c("t_test", "htest")
  return(rval)
}
