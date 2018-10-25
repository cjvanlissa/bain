#' Bayes factors of informative hypotheses for regression models
#'
#' This function computes approximated adjusted fractional Bayes factors for
#' hypothesis tests in regression models.
#'
#'
#' @aliases Bain_regression_cm Bain_regression_cm
#' @param formula A formula that specifies the regression model. See
#' \code{\link{formula}} or \code{\link{lm}} for details.
#' @param data A data frame that contains the variables in the regression
#' model.
#' @param hyp A character string that specifies hypotheses under evaulation.
#' Variable names should match the names of regression predictors. For example,
#' if formula = Y ~ X1 + X2 + X3, variable names should consist of "X1", "X2"
#' and "X3", but cannot contain e.g., "X4". Variable names in one constraint
#' have to be linked by +, -, =, >, <. Different constraints in one hypothesis
#' can be separated by "&". Competing hypotheses can be separated by ';'. For
#' example, hyp = "X1>0&X2>X3;X1=X2=X3=0". See examples and
#' \code{\link{create_matrices}} for details.
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
#' \dontrun{
#'   ##Example 1
#'   #Input
#'   data(cars)
#'
#'   hyp = "speed = 0"
#'   res<-Bain_regression_cm(dist ~ speed, cars, hyp) #run
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
#'   hyp<-"pop15 = dpi = ddpi = 0;
#'         pop15 < 0 & dpi = 0 & ddpi > 0;
#'         pop15 < dpi < ddpi"
#'
#'   #run
#'   res<-Bain_regression_cm(sr ~ pop15 + pop75 + dpi + ddpi, LifeCycleSavings,
#'                           hyp, standardize = TRUE)
#'   ## assume "pop75" is not in the hypothesis.
#'   #Results are printed.
#'   #Results for fit, complexity, Bayes factor, and PMPs are saved in "res":
#'
#'   plot(res)
#'   #Results for PMPs are plotted.
#'   }
Bain_regression_cm<-function(formula, data, hyp, standardize = FALSE){

  dependent<-model.frame(formula,data)[,1]
  predictor<-model.frame(formula,data)[,-1]
  predictor_names<-names(model.frame(formula,data))[-1]
  n<-length(dependent)

  split_temp<-unlist(strsplit(hyp,"[=<>+-]|[\\*\\/\\&\\(\\)\\{\\}\\,\\;]"))
  split_temp<-split_temp[which(grepl("[a-zA-Z_]",split_temp))]
  split_temp<-unique(gsub("\\s+", "",split_temp, perl = TRUE))   ##remove blank; unique

  if(all(split_temp%in%predictor_names)){
    varnames<-predictor_names[which(predictor_names%in%split_temp)]
  }else{
    stop("'hyp' contains invalid parameter(s), which does not match predictor names in 'formula'")
  }

  jointpara<-length(varnames)

  ###standardize
  if(!standardize){
    fit<-lm(formula,data = data)
    estimate <- coef(fit)[varnames]
    covariance <- vcov(fit)[varnames,varnames]
    reg.fit<-fit
  }else{
    sink(tempfile())
    ##Function seBeta() is from fungible package (version 1.5).
    ##It computes standardized estimates of coefficients.
    intermed <- seBeta(predictor, dependent, Nobs = n, alpha = .05, estimator = 'Normal')
    sink()
    var_num <- which(predictor_names %in% varnames)
    estimate <- intermed$CIs$estimate[var_num]
    covariance <- intermed$cov.mat[var_num,var_num]
    reg.fit<-intermed
  }

  TRr<-create_matrices(varnames,hyp)
  IR_chara<-paste0("TRr[[",1:length(TRr),"]]",sep=",",collapse = "")

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

