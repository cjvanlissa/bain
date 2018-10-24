#' Bayes factors of informative hypotheses for regression models
#'
#' This function computes approximated adjusted fractional Bayes factors for
#' hypothesis tests in regression models.
#'
#'
#' @aliases Bain_regression Bain_regression
#' @param formula A formula that specifies the regression model. See
#' \code{\link{formula}} or \code{\link{lm}} for details.
#' @param data A data frame that contains the variables in the regression
#' model.
#' @param ERr A matrix representing equality constraints in a hypothesis. If no
#' equality constraints, set ERr = NULL or ERr = matrix(0,0,0).  See
#' \code{Bain} for details.
#' @param IRr A matrix representing inequality or about equality constraints in
#' a hypothesis. If no inequality and about equality constraints, set IRr =
#' NULL or IRr=matrix(0,0,0). See \code{Bain} for details.
#' @param \dots Sets of matrices representing the equality and/or inequality
#' constraints for the extra informative hypotheses. For example, if there are
#' two informative hypotheses, then ... will be two matrices for the second
#' hypothesis: ERr2 and IRr2. See \code{Bain} for details.
#' @param covariates_hypo A character vector or a formula that specifies names
#' of predictors of which the coefficients are in the hypotheses. For example,
#' if the hypothesis compares cofficients of predictors "Age" and "Sex" of the
#' dependent variable "Income", then covariates_hypo = c("Age", "Sex") or
#' covariates_hypo = ~ Age + Sex. Please note that the order of the variables
#' specified by covariates_hypo should be in line with the restriction matrices
#' specified by ERr and IRr. For example, if covariates_hypo = ~ Age + Sex,
#' then the first column in ERr or IRr should represent the coefficient of Age
#' and the second should represent the coefficient of Sex. The defaul value is
#' covariates_hypo = NULL, which assumes all predictors specified in formula
#' are used in the hypotheses.
#' @param standardize logical. If FALSE (default), then the estimates and
#' covariance matrix of coefficients will not be standardized. If TRUE, then
#' the estimates and covariance matrix of coefficients will be standardsized.
#' When testing whether coefficients are zero, positive, or negative, we
#' suggest standardize = FALSE. When comparing coefficients, we suggest
#' standardize = TRUE. The standardization uses function "seBeta" in package
#' "fungible", see reference for the theory.
#' @return \item{fit}{Fit for each hypothesis under comparison}
#' \item{complexity}{Complexity for each hypothesis under comparison}
#' \item{BF}{Bayes factor for each hypothesis compared to its complement}
#' \item{PMPa}{Posterior probability for each hypothesis excluding
#' unconstrained hypothesis} \item{PMPb}{Posterior probability for each
#' hypothesis including unconstrained hypothesis}
#' @author Xin Gu, Herbert Hoijtink, Joris Mulder
#' @references Jones, J.A. and Waller, N.G. (2015). The normal theory and
#' asymptotic distribution-free (ADF) covariance matrix of standardized
#' regression coefficients: theorical extensions and finite sample behavior.
#' Psychometrika, 80, 365-378.
#' @keywords internal htest
#' @examples
#'
#'   ##Example 1
#'   #Input
#'   data(cars)
#'
#'   #Hypothesis
#'   #H0: theta = 0
#'   ERr<-matrix(c(1,0),nrow=1,ncol=2,byrow = TRUE)
#'   IRr<-NULL
#'
#'   res<-Bain_regression(dist ~ speed, data = cars, ERr, IRr) #run
#'   #Results are printed.
#'   #Results for fit, complexity, Bayes factor, and PMPs are saved in "res":
#'
#'   plot(res)
#'   #Results for PMPs are plotted.
#'
#'
#'   ##Example 2
#'   #Input
#'   data(LifeCycleSavings)
#'
#'   #Hypothesis 1
#'   #H1: theta1 = theta2 = theta3 = 0
#'   ERr1<-matrix(c(1,0,0,0,0,1,0,0,0,0,1,0),nrow=3,ncol=4,byrow = TRUE)
#'   IRr1<-NULL
#'
#'   #H2: theta1 < 0, theta2 = 0, theta3 > 0
#'   ERr2<-matrix(c(0,1,0,0),nrow=1,ncol=4,byrow = TRUE)
#'   IRr2<-matrix(c(-1,0,0,0,0,0,1,0),nrow=2,ncol=4,byrow = TRUE)
#'
#'   #H3: theta1 < theta2 < theta3
#'   ERr3<-NULL
#'   IRr3<-matrix(c(-1,1,0,0,0,-1,1,0),nrow=2,ncol=4,byrow = TRUE)
#'
#'   #run
#'   res<-Bain_regression(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings,
#'   ERr1, IRr1, ERr2, IRr2, ERr3, IRr3,
#'   covariates_hypo = ~ pop15 + dpi + ddpi, standardize = TRUE)
#'   ## assume "pop75" is not in the hypothesis.
#'   #Results are printed.
#'   #Results for fit, complexity, Bayes factor, and PMPs are saved in "res":
#'
#'   plot(res)
#'   #Results for PMPs are plotted.
Bain_regression<-function(formula, data, ERr = NULL, IRr = NULL,...,covariates_hypo = NULL, standardize = FALSE){
  TRr<-list(ERr,IRr,...)
  IR_chara<-paste0("TRr[[",1:length(TRr),"]]",sep=",",collapse = "")

  dependent<-model.frame(formula,data)[,1]
  predictor<-model.frame(formula,data)[,-1]
  predictor_names<-names(model.frame(formula,data))[-1]
  n<-length(dependent)

  if(is.null(covariates_hypo)){ ##If null, assume all predictors are used in the hypotheses
    covariates_hypo<-names(model.frame(formula,data))[-1]
  }

  if(inherits(covariates_hypo,"formula")){  ## if formula
    covariates_hypo<-names(model.frame(covariates_hypo,data))
  }

  if(!is.character(covariates_hypo)){stop("'covariates_hypo' should be a character (vector), formula, or NULL")}
  if(!all(covariates_hypo %in% predictor_names)){
    stop("'covariates_hypo' should be a subset of predictors in 'formula'")
  }

  jointpara<-length(covariates_hypo)

  ###standardize
  if(!standardize){
    fit<-lm(formula,data = data)
    estimate <- coef(fit)[covariates_hypo]
    covariance <- vcov(fit)[covariates_hypo,covariates_hypo]
    reg.fit<-fit
  }else{
    sink(tempfile())
    ##Function seBeta() is from fungible package (version 1.5).
    ##It computes standardized estimates of coefficients.
    intermed <- seBeta(predictor, dependent, Nobs = n, alpha = .05, estimator = 'Normal')
    sink()
    covariates_hypo <- which(predictor_names %in% covariates_hypo)
    estimate <- intermed$CIs$estimate[covariates_hypo]
    covariance <- intermed$cov.mat[covariates_hypo,covariates_hypo]
    reg.fit<-intermed
  }

  Bain_chara<-paste0("Bain(estimate,covariance,grouppara=0,jointpara=jointpara,n=n,",IR_chara,"seed=100,print=FALSE)")
  Bain_res<-eval(parse(text = Bain_chara))
  BFmatrix<-as.matrix(Bain_res$BFmatrix)

  cl<-match.call()

  cat("Regression test result", sep="\n")
  write.table(capture.output(Bain_res$fit_com_table[,5:9]),col.names = FALSE,row.names = FALSE,quote = FALSE)

  writeLines(" ")
  cat("BF-matrix", sep="\n")
  write.table(capture.output(data.frame(formatC(BFmatrix, digits = 3, format = "f"))),col.names = FALSE,row.names = FALSE,quote = FALSE)
  writeLines(" ")

  Regtest_res<-Bain_res$testResult
  #reg_res<-list(test_res = Regtest_res, estimate_res = reg.fit)
  reg_res<-list(fit = Regtest_res$fit, complexity = Regtest_res$complexity,
                BF = Regtest_res$BF, PMPa = Regtest_res$PMPa, PMPb = Regtest_res$PMPb,
                estimate_res = reg.fit)


  class(reg_res)<-"Bain"
  reg_res$call<-cl
  return(invisible(reg_res))
}

