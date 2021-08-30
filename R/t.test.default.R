# Student's t-Test
#
# Performs one and two sample t-tests on vectors of data.
#
# @details The formula interface is only applicable for the 2-sample tests.
#
# \code{alternative = "greater"} is the alternative that \code{x} has a larger
# mean than \code{y}.
#
# If \code{paired} is \code{TRUE} then both \code{x} and \code{y} must be
# specified and they must be the same length.  Missing values are silently
# removed (in pairs if \code{paired} is \code{TRUE}).  If \code{var.equal} is
# \code{TRUE} then the pooled estimate of the variance is used.  By default,
# if \code{var.equal} is \code{FALSE} then the variance is estimated
# separately for both groups and the Welch modification to the degrees of
# freedom is used.
#
# If the input data are effectively constant (compared to the larger of the
# two means) an error is generated.
#
# @section Bain t_test_old:
# In order to allow users to enjoy the functionality of bain with the familiar
# stats-function t.test, we have had to make minor changes to the function
# t.test.default. All rights to, and credit for, the function t.test.default
# belong to the R Core Team, as indicated in the original license below.
# We make no claims to copyright and incur no liability with regard to the
# changes implemented in t_test_old.
#
# This the original copyright notice by the R core team:
# File src/library/stats/R/t_test_old.R
# Part of the R package, https://www.R-project.org
#
# Copyright (C) 1995-2015 The R Core Team
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# A copy of the GNU General Public License is available at
# https://www.R-project.org/Licenses/
#
# @aliases t_test_old t_test_old.default t_test_old.formula
# @param x a (non-empty) numeric vector of data values.
# @param \dots further arguments to be passed to or from methods.
# @return A list with class \code{"htest"} containing the following
# components: \item{statistic}{the value of the t-statistic.}
# \item{parameter}{the degrees of freedom for the t-statistic.}
# \item{p.value}{the p-value for the test.} \item{conf.int}{a confidence
# interval for the mean appropriate to the specified alternative hypothesis.}
# \item{estimate}{the estimated mean or difference in means depending on
# whether it was a one-sample test or a two-sample test.}
# \item{null.value}{the specified hypothesized value of the mean or mean
# difference depending on whether it was a one-sample test or a two-sample
# test.} \item{alternative}{a character string describing the alternative
# hypothesis.} \item{method}{a character string indicating what type of t-test
# was performed.} \item{data.name}{a character string giving the name(s) of
# the data.}
# @seealso \code{\link[stats]{t.test}}
# @keywords internal
# @examples
#
# require(graphics)
#
# t_test_old(1:10, y = c(7:20))      # P = .00001855
# t_test_old(1:10, y = c(7:20, 200)) # P = .1245    -- NOT significant anymore
#
# ## Classical example: Student's sleep data
# plot(extra ~ group, data = sleep)
# ## Traditional interface
# with(sleep, t_test_old(extra[group == 1], extra[group == 2]))
# ## Formula interface
# t_test_old(extra ~ group, data = sleep)

t_test_old <- function(x, ...) UseMethod("t_test_old")

# @method t_test_old default
# @rdname t_test_old
# @param y an optional (non-empty) numeric vector of data values.
# @param alternative a character string specifying the alternative hypothesis,
# must be one of \code{"two.sided"} (default), \code{"greater"} or
# \code{"less"}.  You can specify just the initial letter.
# @param mu a number indicating the true value of the mean (or difference in
# means if you are performing a two sample test).
# @param paired a logical indicating whether you want a paired t-test.
# @param var.equal a logical variable indicating whether to treat the two
# variances as being equal. If \code{TRUE} then the pooled variance is used to
# estimate the variance otherwise the Welch (or Satterthwaite) approximation
# to the degrees of freedom is used.
# @param conf.level confidence level of the interval.
# @export
t_test_old.default <-
  function(x, y = NULL, alternative = c("two.sided", "less", "greater"),
           mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95,
           ...)
  {

    alternative <- match.arg(alternative)

    if(!missing(mu) && (length(mu) != 1 || is.na(mu)))
      stop("'mu' must be a single number")
    if(!missing(conf.level) &&
       (length(conf.level) != 1 || !is.finite(conf.level) ||
        conf.level < 0 || conf.level > 1))
      stop("'conf.level' must be a single number between 0 and 1")
    if( !is.null(y) ) {
      dname <- paste(deparse(substitute(x)),"and",
                     deparse(substitute(y)))
      if(paired)
        xok <- yok <- complete.cases(x,y)
      else {
        yok <- !is.na(y)
        xok <- !is.na(x)
      }
      y <- y[yok]
    }
    else {
      dname <- deparse(substitute(x))
      if (paired) stop("'y' is missing for paired test")
      xok <- !is.na(x)
      yok <- NULL
    }
    x <- x[xok]
    if (paired) {
      x <- x-y
      y <- NULL
    }
    nx <- length(x)
    mx <- mean(x)
    vx <- var(x)
    if(is.null(y)) {
      if(nx < 2) stop("not enough 'x' observations")
      df <- nx-1
      stderr <- sqrt(vx/nx)
      if(stderr < 10 *.Machine$double.eps * abs(mx))
        stop("data are essentially constant")
      tstat <- (mx-mu)/stderr
      method <- if(paired) "Paired t_test" else "One Sample t_test"
      estimate <-
        setNames(mx, if(paired)"mean of the differences" else "mean of x")
    } else {
      ny <- length(y)
      if(nx < 1 || (!var.equal && nx < 2))
        stop("not enough 'x' observations")
      if(ny < 1 || (!var.equal && ny < 2))
        stop("not enough 'y' observations")
      if(var.equal && nx+ny < 3) stop("not enough observations")
      my <- mean(y)
      vy <- var(y)
      method <- paste(if(!var.equal)"Welch", "Two Sample t_test")
      estimate <- c(mx,my)
      names(estimate) <- c("mean of x","mean of y")
      if(var.equal) {
        df <- nx+ny-2
        v <- 0
        if(nx > 1) v <- v + (nx-1)*vx
        if(ny > 1) v <- v + (ny-1)*vy
        v <- v/df
        stderr <- sqrt(v*(1/nx+1/ny))
      } else {
        stderrx <- sqrt(vx/nx)
        stderry <- sqrt(vy/ny)
        stderr <- sqrt(stderrx^2 + stderry^2)
        df <- stderr^4/(stderrx^4/(nx-1) + stderry^4/(ny-1))
      }
      if(stderr < 10 *.Machine$double.eps * max(abs(mx), abs(my)))
        stop("data are essentially constant")
      tstat <- (mx - my - mu)/stderr
    }
    if (alternative == "less") {
      pval <- pt(tstat, df)
      cint <- c(-Inf, tstat + qt(conf.level, df) )
    }
    else if (alternative == "greater") {
      pval <- pt(tstat, df, lower.tail = FALSE)
      cint <- c(tstat - qt(conf.level, df), Inf)
    }
    else {
      pval <- 2 * pt(-abs(tstat), df)
      alpha <- 1 - conf.level
      cint <- qt(1 - alpha/2, df)
      cint <- tstat + c(-cint, cint)
    }
    cint <- mu + cint * stderr
    names(tstat) <- "t"
    names(df) <- "df"
    names(mu) <- if(paired || !is.null(y)) "difference in means" else "mean"
    attr(cint,"conf.level") <- conf.level
    # The following lines have been changed by Caspar van Lissa,
    # package maintainer of bain
    # Original:
    # rval <- list(statistic = tstat, parameter = df, p.value = pval,
    #              conf.int = cint, estimate = estimate, null.value = mu,
    #              alternative = alternative,
    #              method = method, data.name = dname)
    # class(rval) <- "htest"
    # Substituted by:
    rval <- list(statistic = tstat, parameter = df, p.value = pval,
                 conf.int = cint, estimate = estimate, null.value = mu, alternative = alternative,
                 method = method, data.name = dname);
    if(!is.null(y)&!paired){
      rval$n <- c(nx, ny)
      rval$v <- c(vx, vy)
    } else {
      rval$n <- nx
      rval$v <- vx
    }
    class(rval) <- c("t_test", "htest")
    return(rval)
  }


# @importFrom stats terms
# @method t_test_old formula
# @rdname t_test_old
# @param formula a formula of the form \code{lhs ~ rhs} where \code{lhs} is a
# numeric variable giving the data values and \code{rhs} a factor with two
# levels giving the corresponding groups.
# @param data an optional matrix or data frame (or similar: see
# \code{\link{model.frame}}) containing the variables in the formula
# \code{formula}.  By default the variables are taken from
# \code{environment(formula)}.
# @param subset an optional vector specifying a subset of observations to be
# used.
# @param na.action a function which indicates what should happen when the data
# contain \code{NA}s.  Defaults to \code{getOption("na.action")}.
# @export
t_test_old.formula <- function(formula, data, subset, na.action, ...)
{
  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  m$... <- NULL
  mf <- eval(m, parent.frame())
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  g <- factor(mf[[-response]])
  if (nlevels(g) != 2L)
    stop("grouping factor must have exactly 2 levels")
  DATA <- setNames(split(mf[[response]], g), c("x", "y"))
  y <- do.call(t_test_old.default, c(DATA, list(...)))
  y$data.name <- DNAME
  if (length(y$estimate) == 2L)
    names(y$estimate) <- paste0("mean of group", levels(g))
  y
}



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
