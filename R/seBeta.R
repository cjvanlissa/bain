##This function is from fungible package (version 1.5).
##The fungible package has been withdrawn in CRAN.
##The tile of this function is Standard Errors and CIs for Standardized Regression Coefficients
##It computes normal theory and ADF standard errors and CIs for standardized regression coefficients
##The authors are Jeff Jones and Niels Waller.
##The reference paper is
##Jones, J. A, and Waller, N. G. (2015).
##The Normal-Theory and Asymptotic Distribution-Free (ADF) covariance matrix of
##standardized regression coefficients: Theoretical extensions and finite sample behavior.
##Psychometrika, 80, 365-378.



#' Standard Errors and CIs for Standardized Regression Coefficients
#'
#' Computes Normal Theory and ADF Standard Errors and CIs for Standardized
#' Regression Coefficients
#'
#'
#' @aliases seBeta seBeta
#' @param X Matrix of predictor scores.
#' @param y Vector of criterion scores.
#' @param cov.x Covariance or correlation matrix of predictors.
#' @param cov.xy Vector of covariances or correlations between predictors and
#' criterion.
#' @param var.y Criterion variance.
#' @param Nobs Number of observations.
#' @param alpha Desired Type I error rate; default = .05.
#' @param estimator 'ADF' or 'Normal' confidence intervals - requires raw X and
#' raw y; default = 'ADF'.
#' @return \item{cov.Beta}{ Normal theory or ADF covariance matrix of
#' standardized regression coefficients.  } \item{se.Beta}{ standard errors for
#' standardized regression coefficients.  } \item{alpha}{ desired Type-I error
#' rate.  } \item{CI.Beta}{ Normal theory or ADF (1-alpha)% confidence
#' intervals for standardized regression coefficients.  } \item{estimator}{
#' estimator = "ADF" or "Normal".  }
#' @author Jeff Jones and Niels Waller
#' @references Jones, J. A, and Waller, N. G. (2015). The Normal-Theory and
#' Asymptotic Distribution-Free (ADF) covariance matrix of standardized
#' regression coefficients: Theoretical extensions and finite sample behavior.
#' Psychometrika, 80, 365-378.
#' @keywords Statistics
#' @examples
#'
#' library(MASS)
#'
#' set.seed(123)
#'
#' R <- matrix(.5, 3, 3)
#' diag(R) <- 1
#' X <- mvrnorm(n = 200, mu = rep(0, 3), Sigma = R, empirical = TRUE)
#' Beta <- c(.2, .3, .4)
#' y <- X %*% Beta + .64 * scale(rnorm(200))
#' results <- seBeta(X, y, Nobs = 200, alpha = .05, estimator = 'ADF')
#' print(results, digits = 3)
#'
#' # 95% CIs for Standardized Regression Coefficients:
#' #
#' #        lbound estimate ubound
#' # beta_1  0.104    0.223  0.341
#' # beta_2  0.245    0.359  0.473
#' # beta_3  0.245    0.360  0.476
#' @export
seBeta<-function (X = NULL, y = NULL, cov.x = NULL, cov.xy = NULL, var.y = NULL,
                  Nobs = NULL, alpha = 0.05, estimator = "ADF")
{

  adfCOV <- function(X, y) {
    dev <- scale(cbind(X, y), scale = FALSE)
    nvar <- ncol(dev)
    N <- nrow(dev)
    ue <- nvar * (nvar + 1)/2
    s <- vector(length = ue, mode = "character")
    z <- 0
    for (i in 1:nvar) {
      for (j in i:nvar) {
        z <- z + 1
        s[z] <- paste(i, j, sep = "")
      }
    }
    v <- expand.grid(s, s)
    V <- paste(v[, 1], v[, 2], sep = "")
    id.mat <- matrix(0, nrow = ue^2, 4)
    for (i in 1:4) id.mat[, i] <- as.numeric(sapply(V, substr,
                                                    i, i))
    M <- matrix(1:ue^2, ue, ue, byrow = TRUE)
    r <- M[lower.tri(M, diag = TRUE)]
    ids <- id.mat[r, ]
    adfCovMat <- matrix(0, ue, ue)
    covs <- matrix(0, nrow(ids), 1)
    for (i in 1:nrow(ids)) {
      w_ij <- crossprod(dev[, ids[i, 1]], dev[, ids[i,
                                                    2]])/N
      w_ik <- crossprod(dev[, ids[i, 1]], dev[, ids[i,
                                                    3]])/N
      w_il <- crossprod(dev[, ids[i, 1]], dev[, ids[i,
                                                    4]])/N
      w_jk <- crossprod(dev[, ids[i, 2]], dev[, ids[i,
                                                    3]])/N
      w_jl <- crossprod(dev[, ids[i, 2]], dev[, ids[i,
                                                    4]])/N
      w_kl <- crossprod(dev[, ids[i, 3]], dev[, ids[i,
                                                    4]])/N
      w_ijkl <- (t(dev[, ids[i, 1]] * dev[, ids[i, 2]]) %*%
                   (dev[, ids[i, 3]] * dev[, ids[i, 4]])/N)
      covs[i] <- (N * (N - 1) * (1/((N - 2) * (N - 3))) *
                    (w_ijkl - w_ij * w_kl) - N * (1/((N - 2) * (N -
                                                                  3))) * (w_ik * w_jl + w_il * w_jk - (2/(N - 1)) *
                                                                            w_ij * w_kl))
    }
    adfCovMat[lower.tri(adfCovMat, diag = T)] <- covs
    vars <- diag(adfCovMat)
    adfCovMat <- adfCovMat + t(adfCovMat) - diag(vars)
    adfCovMat
  }
  vech <- function(x) t(x[!upper.tri(x)])
  Dn <- function(x) {
    mat <- diag(x)
    index <- seq(x * (x + 1)/2)
    mat[lower.tri(mat, TRUE)] <- index
    mat[upper.tri(mat)] <- t(mat)[upper.tri(mat)]
    outer(c(mat), index, function(x, y) ifelse(x == y, 1,
                                               0))
  }
  DIAG <- function(x = 1, nrow, ncol) {
    if (length(x) == 1)
      x <- as.matrix(x)
    if (is.matrix(x))
      return(diag(x))
    else return(diag(x, nrow, ncol))
  }
  if ((is.null(X) | is.null(y)) & estimator == "ADF")
    stop("\nYou need to supply both X and y for ADF Estimation\n")
  if (is.null(X) & !is.null(y))
    stop("\n y is not defined\n Need to specify both X and y\n")
  if (!is.null(X) & is.null(y))
    stop("\n X is not defined\n Need to specify both X and y\n")
  if (is.null(X) & is.null(y)) {
    if (is.null(cov.x) | is.null(cov.xy) | is.null(var.y) |
        is.null(Nobs))
      stop("\nYou need to specify covariances and sample size\n")
    scov <- rbind(cbind(cov.x, cov.xy), c(cov.xy, var.y))
    N <- Nobs
    p <- nrow(cov.x)
  }
  else {
    X <- as.matrix(X)
    y <- as.matrix(y)
    scov <- cov(cbind(X, y))
    N <- length(y)
    p <- ncol(X)
  }
  if (estimator == "ADF") {
    cov.cov <- adfCOV(X, y)
  }
  else {
    Kp.lft <- solve(t(Dn(p + 1)) %*% Dn(p + 1)) %*% t(Dn(p +
                                                           1))
    cov.cov <- 2 * Kp.lft %*% (scov %x% scov) %*% t(Kp.lft)
  }
  param <- c(vech(scov))
  ncovs <- length(param)
  if (p == 1) {
    v.x.pl <- 1
  }
  else {
    v.x.pl <- c(1, rep(0, p - 1))
    for (i in 2:p) v.x.pl[i] <- v.x.pl[i - 1] + p - (i -
                                                       2)
  }
  cx <- scov[1:p, 1:p]
  cxy <- scov[1:p, p + 1]
  vy <- scov[p + 1, p + 1]
  sx <- sqrt(DIAG(cx))
  sy <- sqrt(vy)
  bu <- solve(cx) %*% cxy
  ncx <- length(vech(cx))
  db <- matrix(0, p, ncovs)
  V <- matrix(0, p, ncx)
  V[as.matrix(cbind(1:p, v.x.pl))] <- 1
  db[, 1:ncx] <- (DIAG(c(solve(DIAG(2 * sx * sy)) %*% bu)) %*%
                    V - DIAG(sx/sy) %*% (t(bu) %x% solve(cx)) %*% Dn(p))
  db[, (ncx + 1):(ncx + p)] <- DIAG(sx/sy) %*% solve(cx)
  db[, ncovs] <- -DIAG(sx/(2 * sy^3)) %*% bu
  cx.nms <- matrix(0, p, p)
  cxy.nms <- c(rep(0, p), "var_y")
  for (i in 1:p) for (j in 1:p) cx.nms[i, j] <- paste("cov_x",
                                                      i, "x", j, sep = "")
  for (i in 1:p) cxy.nms[i] <- paste("cov_x", i, "y", sep = "")
  old.ord <- c(vech(cx.nms), cxy.nms)
  new.ord <- vech(rbind(cbind(cx.nms, cxy.nms[1:p]), c(cxy.nms)))
  db <- db[, match(new.ord, old.ord)]
  if (p == 1)
    DEL.cmat <- t(db) %*% cov.cov %*% db/N
  else DEL.cmat <- db %*% cov.cov %*% t(db)/N
  b.nms <- NULL
  for (i in 1:p) b.nms[i] <- paste("beta_", i, sep = "")
  rownames(DEL.cmat) <- colnames(DEL.cmat) <- b.nms
  DELse <- sqrt(DIAG(DEL.cmat))
  CIs <- as.data.frame(matrix(0, p, 3))
  colnames(CIs) <- c("lbound", "estimate", "ubound")
  for (i in 1:p) rownames(CIs)[i] <- paste("beta_", i, sep = "")
  tc <- qt(alpha/2, N - p - 1, lower.tail = F)
  beta <- DIAG(sx) %*% bu * sy^-1
  for (i in 1:p) {
    CIs[i, ] <- c(beta[i] - tc * DELse[i], beta[i], beta[i] +
                    tc * DELse[i])
  }

  out <- list(cov.mat = DEL.cmat, SEs = DELse, alpha = alpha,
              CIs = CIs, estimator = estimator)
  class(out) <- c("seBeta", "list")
  out
}



#' @method print seBeta
#' @export
print.seBeta <- function(x, digits = getOption("digits"), ...){
  cat("\n", 100 * (1 - x$alpha), "% CIs for Standardized Regression Coefficients:\n\n",
      sep = "")
  print(round(x$CIs, digits))
}
