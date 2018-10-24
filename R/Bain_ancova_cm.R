#' Bayes factors of informative hypotheses for ANCOVA models (user-friendly
#' hypotheses input)
#'
#' This function computes approximated adjusted fractional Bayes factors for
#' ancova test with user-friendly hypotheses input.
#'
#'
#' @aliases Bain_ancova_cm Bain_ancova_cm
#' @param X A data frame that contains dependent variable, covariates, and
#' group factor.
#' @param dep_var A character that indicates the name of the dependent variable
#' in X. Only one dependent variable can be specified.
#' @param covariates A character vector that indicates names of covariates in
#' X.
#' @param group A character that indicates the name of the group factor in X.
#' Only one group factor can be specified.
#' @param hyp A character string that specifies hypotheses under evaulation.
#' Variable names in the hypotheses have to be specified as 'group.n' with n =
#' 1,2,3... denoting group numbers (e.g., group.1, group.2 and group.3), or
#' 'group.category' (e.g., Sex.M and Sex.F for group factor 'Sex' with two
#' categories 'M' and 'F'). Group/categpory names can contain: letters,
#' numbers, and _. Variable names in one constraint have to be linked by +, -,
#' =, >, <, e.g., group.1=group.2>group.3, or Sex.M<Sex.F. Different
#' constraints in one hypothesis can be separated by "&", e.g., hyp =
#' "Sex.F>0&Sex.M<0". Competing hypotheses can be separated by ';', e.g., hyp =
#' "Sex.M=Sex.F; Sex.M>Sex.F". See examples and \code{\link{create_matrices}}
#' for details.
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
#'   data(ChickWeight)
#'   #Hypotheses
#'   hyp<-"Diet.1=Diet.2=Diet.3=Diet.4;
#'         Diet.1<Diet.2<Diet.3<Diet.4;
#'         Diet.1<Diet.2<Diet.4<Diet.3"
#'
#'   res<-Bain_ancova_cm(ChickWeight,"weight","Time","Diet",hyp) #run
#'   #Results are printed.
#'   #Results for fit, complexity, Bayes factor, and PMPs are saved in "res":
#'
#'   plot(res)
#'   #Results for PMPs are plotted.
#'
Bain_ancova_cm<-function(X, dep_var, covariates, group, hyp){

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

  ##possible variable names
  varnames_g<-paste("g",1:length(n),sep = "")
  varnames_gd<-paste("g",1:length(n),sep = ".")
  varnames_gn<-paste(group,1:length(n),sep = ".")
  varnames_gf<-paste(group,levels(groupf),sep = ".")

  split_temp<-unlist(strsplit(hyp,"[=<>+-]|[\\*\\/\\&\\(\\)\\{\\}\\,\\;]"))
  split_temp<-split_temp[which(grepl("[a-zA-Z]",split_temp))]
  split_temp<-unique(gsub("\\s+", "",split_temp, perl = TRUE))   ##remove blank; unique

  if(all(split_temp%in%varnames_g)){
    varnames<-varnames_g
  }else if(all(split_temp%in%varnames_gd)){
    varnames<-varnames_gd
  }else if(all(split_temp%in%varnames_gn)){
    varnames<-varnames_gn
  }else if(all(split_temp%in%varnames_gf)){
    varnames<-varnames_gf
  }else{
    stop("'hyp' contains invalid parameter(s), which does not match group factor")
  }

  TRr<-create_matrices(varnames,hyp)

  ##check
  if(any(unlist(lapply(TRr,ncol))!=(length(levels(groupf))+1)&unlist(lapply(TRr,ncol))!=0)){
    stop("Number of columns in ERr or IRr should equal number of groups plus 1.")
  }

  ##hypothesis
  newTRr<- lapply(TRr,.add_col,n_cov=n_covars)## add extra columns for covariates' coeffs.
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


.add_col<-function(x,n_cov){
  if(length(x)!=0) x<-cbind(x, matrix(0,nrow(x),n_cov))
  return(x)
}

