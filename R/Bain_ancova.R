#' Bayes factors of informative hypotheses for ANCOVA models
#'
#' This function computes approximated adjusted fractional Bayes factors for
#' ancova test.
#'
#' Be aware that the constraints matrices ERr/IRr have to be constructed in
#' accordance with the order of the groups according to the lm function. For
#' example, if the groups are labelled a1, a2, a3, then the order is clear, but
#' if the groups are labelled yellow, blue and gray, then the order is blue,
#' gray, yellow.
#'
#' @aliases Bain_ancova Bain_ancova
#' @param X A data frame that contains dependent variable, covariates, and
#' group factor.
#' @param dep_var A character that indicates the name of the dependent variable
#' in X. Only one dependent variable can be specified.
#' @param covariates A character vector that indicates names of covariates in
#' X.
#' @param group A character that indicates the name of the group factor in X.
#' Only one group factor can be specified.
#' @param ERr A matrix representing equality constraints in a hypothesis. If no
#' equality constraints, set ERr = NULL or ERr = matrix(0,0,0).  See
#' \code{Bain} for details.  Number of columns should equal to number of
#' groups plus 1. If one or more groups are not included in the hypothesis,
#' please specify corresponding columns with zeros.
#' @param IRr A matrix representing inequality or about equality constraints in
#' a hypothesis. If no inequality and about equality constraints, set IRr =
#' NULL or IRr = matrix(0,0,0). See \code{Bain} for details.  Number of
#' columns should equal to number of groups plus 1. If one or more groups are
#' not included in the hypothesis, please specify corresponding columns with
#' zeros.
#' @param \dots Sets of matrices representing the equality and/or inequality
#' constraints for the extra informative hypotheses. For example, if there are
#' two informative hypotheses, then ... will be two matrices for the second
#' hypothesis: ERr2 and IRr2. See \code{Bain} for details.
#' @return \item{fit}{Fit for each hypothesis under comparison}
#' \item{complexity}{Complexity for each hypothesis under comparison}
#' \item{BF}{Bayes factor for each hypothesis compared to its complement}
#' \item{PMPa}{Posterior probability for each hypothesis excluding
#' unconstrained hypothesis} \item{PMPb}{Posterior probability for each
#' hypothesis including unconstrained hypothesis}
#' @author Xin Gu, Herbert Hoijtink, Joris Mulder
#' @keywords internal htest
#' @examples
#'
#'   #Input
#'   #Build in data: Weight versus age (Time) of chicks on different diets (Diet).
#'   data(ChickWeight)
#'
#'   #Hypotheses
#'
#'   #H1: theta1=theta2=theta3=theta4
#'   ERr1<-matrix(c(1,-1,0,0,0,0,1,-1,0,0,0,0,1,-1,0),nrow=3,ncol=5,byrow = TRUE)
#'   IRr1<-NULL
#'
#'   #H2: theta1<theta2<theta3<theta4
#'   ERr2<-NULL
#'   IRr2<-matrix(c(-1,1,0,0,0,0,-1,1,0,0,0,0,-1,1,0),nrow=3,ncol=5,byrow = TRUE)
#'
#'   #H3: theta1<theta2<theta4<theta3
#'   ERr3<-NULL
#'   IRr3<-matrix(c(-1,1,0,0,0,0,-1,0,1,0,0,0,1,-1,0),nrow=3,ncol=5,byrow = TRUE)
#'
#'   res<-Bain_ancova(ChickWeight,"weight","Time","Diet",ERr1,IRr1,ERr2,IRr2,ERr3,IRr3) #run
#'   #Results are printed.
#'   #Results for fit, complexity, Bayes factor, and PMPs are saved in "res":
#'
#'   plot(res)
#'   #Results for PMPs are plotted.
Bain_ancova<-function(X, dep_var, covariates, group, ERr=NULL,IRr=NULL,...){
  TRr<-list(ERr,IRr,...)

  if(!is.data.frame(X)){stop("X should be a data frame")}  #X data frame
  Xnames<-names(X)

  if(!(is.character(dep_var)&&is.character(covariates)&&is.character(group)) ){
    stop("dep_var, covariates, and group should be character")
  }
  if(length(dep_var)!=1){stop("Please specify only one dependent variable")}
  if(length(group)!=1){stop("Please specify only one group factor")}

  var_names<-c(dep_var,covariates,group)
  if(any(!var_names%in%Xnames)){stop("one of names of dep_var, covariates, and group is not in X")}

  depv<-X[,dep_var]  ##dependent variable
  groupf<-factor(X[,group]) ## group factor
  covars<-apply(as.matrix(X[,covariates]),2,function(x) x-mean(x))  ##standardized covariates
  n_covars=length(covariates)

  n<-unlist(lapply(split(depv,groupf),length)) ##sample size per group

  ##analysis
  ancovafm <-  lm(depv ~ groupf + covars -1)
  estimate<-coef(ancovafm)
  resvar<-(summary(ancovafm)$sigma)**2

  newdata<-cbind(rep(1,nrow(covars)),covars) # IMPORTANT NOW FOR EACH GROUP THE INTERCEPT IS ADDED

  split_index<-split(1:nrow(covars),groupf)
  split_newdata<-lapply(split_index, function(x) newdata[x,])
  covariance<-lapply(split_newdata, function(x) resvar * solve(t(x) %*% x))

  ##check
  if(any(unlist(lapply(TRr,ncol))!=(length(levels(groupf))+1)&unlist(lapply(TRr,ncol))!=0)){
    stop("Number of columns in ERr or IRr should equal number of groups plus 1.")
  }

  ##hypothesis
  newTRr<- lapply(TRr,add_col,n_cov=n_covars)## add extra columns for covariates' coeffs.
  IR_chara<-paste0(",newTRr[[",1:length(newTRr),"]]",sep="",collapse = "")


  jointpara<-n_covars
  ##check
  if(jointpara!=(length(estimate)-length(n))){stop("length(n) + n_covars != length(estimate)")}

  Bain_chara<-paste0("Bain(estimate,covariance,grouppara=1,jointpara=jointpara,n=n",IR_chara,",seed=100,print=FALSE)")
  Bain_res<-eval(parse(text = Bain_chara))
  BFmatrix<-as.matrix(Bain_res$BFmatrix)


  cat("ANCOVA test result", sep="\n")
  write.table(capture.output(Bain_res$fit_com_table[,5:9]),col.names = FALSE,row.names = FALSE,quote = FALSE)

  writeLines(" ")
  cat("BF-matrix", sep="\n")
  write.table(capture.output(data.frame(formatC(BFmatrix, digits = 3, format = "f"))),col.names = FALSE,row.names = FALSE,quote = FALSE)
  writeLines(" ")

  ancovatest_res<-Bain_res$testResult

  ancova_res<-list(fit = ancovatest_res$fit, complexity = ancovatest_res$complexity,
                   BF = ancovatest_res$BF, PMPa = ancovatest_res$PMPa, PMPb = ancovatest_res$PMPb,
                   estimate_res = ancovafm)

  cl<-match.call()
  class(ancova_res)<-"Bain"
  ancova_res$call<-cl

  return(invisible(ancova_res))
}


add_col<-function(x,n_cov){
  if(length(x)!=0) x<-cbind(x, matrix(0,nrow(x),n_cov))
  return(x)
}

