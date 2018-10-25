#' Bayes factors of informative hypotheses for ANOVA models
#'
#' This function computes approximated adjusted fractional Bayes factors for
#' anova test.
#'
#' Be aware that the constraints matrices ERr/IRr have to be constructed in
#' accordance with the order of the groups according to the lm function. For
#' example, if the groups are labelled a1, a2, a3, then the order is clear, but
#' if the groups are labelled yellow, blue and gray, then the order is blue,
#' gray, yellow.
#'
#' @aliases Bain_anova Bain_anova
#' @param X A data frame that contains dependent variable and group factor.
#' @param dep_var A character that indicates the name of the dependent variable
#' in X.
#' @param group A character that indicates the name of the group factor in X
#' @param ERr A matrix representing equality constraints in a hypothesis. If no
#' equality constraints, set ERr = NULL or ERr = matrix(0,0,0).  See
#' \code{Bain} for details.
#' @param IRr A matrix representing inequality or about equality constraints in
#' a hypothesis. If no inequality and about equality constraints, set IRr =
#' NULL or IRr = matrix(0,0,0). See \code{Bain} for details.
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
#' \dontrun{
#'   #Hypotheses
#'   #H1: theta1=theta2=theta3   #group means
#'   #H2: theta1>theta2>theta3
#'   #H3: theta1<theta2<theta3
#'
#'   #Input
#'   X<-data.frame(c(rnorm(20,0,1),rnorm(40,.5,2),rnorm(60,1,5)),
#'                 c(rep(1,20),rep(2,40),rep(3,60)))
#'   names(X)<-c("dep_var","group")
#'   head(X)
#'   tail(X)
#'
#'   #H1:
#'   ERr1<-matrix(c(1,-1,0,0,0,1,-1,0),nrow=2,ncol=4,byrow = TRUE)
#'   IRr1<-NULL
#'
#'   #H2
#'   ERr2<-NULL
#'   IRr2<-matrix(c(1,-1,0,0,0,1,-1,0),nrow=2,ncol=4,byrow = TRUE)
#'
#'   #H3
#'   ERr3<-NULL
#'   IRr3<-matrix(c(-1,1,0,0,0,-1,1,0),nrow=2,ncol=4,byrow = TRUE)
#'
#'   res<-Bain_anova(X,"dep_var","group",ERr1,IRr1,ERr2,IRr2,ERr3,IRr3) #run
#'   #Results are printed.
#'   #Results for fit, complexity, Bayes factor, and PMPs are saved in "res":
#'
#'   plot(res)
#'   #Results for PMPs are plotted.
#' }
#' @export
Bain_anova<-function(X, dep_var=NULL, group=NULL, ERr=NULL,IRr=NULL, ...){
  TRr<-list(ERr,IRr,...)
  IR_chara<-paste0("TRr[[",1:length(TRr),"]]",sep=",",collapse = "")

  if(!is.data.frame(X)){stop("X should be a data frame")}  #X data frame

  if(is.character(dep_var)){
    if(length(dep_var)!=1){stop("Please specify only one dependent variable")}
    temp<-which(names(X)==dep_var)
    if(length(temp)!=1){stop("No variable or two or more variables has name dep_var in X")}
    depv <- X[,temp]         ###dependent variable
  }else if(is.null(dep_var)){
    depv <- X[,1]
  }else{
    stop("dep_var should be a character or NULL")
  }

  if(is.character(group)){
    temp<-which(names(X)==group)
    if(length(temp)!=1){stop("No variable or two or more variables has name group in X")}
    groupf<- factor(X[,temp])   ###dgroup factor
  }else if(is.null(group)){
    groupf <- factor(X[,2])
  }else{
    stop("group should be a character or NULL")
  }

  n<-unlist(lapply(split(depv,groupf),length))

  anovafm <-  lm(depv ~ groupf -1)
  estimate<-coef(anovafm)
  variance <- (summary(anovafm)$sigma)**2
  variance <- 1/n * variance
  covlist<- lapply(as.list(variance),matrix) ##convert variances to list of variances used in Bain

  Bain_chara<-paste0("Bain(estimate,covlist,grouppara=1,jointpara=0,n=n,",IR_chara,"seed=100,print=FALSE)")
  Bain_res<-eval(parse(text = Bain_chara))
  BFmatrix<-as.matrix(Bain_res$BFmatrix)


  cat("ANOVA test result", sep="\n")
  write.table(capture.output(Bain_res$fit_com_table[,5:9]),col.names = FALSE,row.names = FALSE,quote = FALSE)

  writeLines(" ")
  cat("BF-matrix", sep="\n")
  write.table(capture.output(data.frame(formatC(BFmatrix, digits = 3, format = "f"))),col.names = FALSE,row.names = FALSE,quote = FALSE)
  writeLines(" ")

  anovatest_res<-Bain_res$testResult

  anova_res<-list(fit = anovatest_res$fit, complexity = anovatest_res$complexity,
                  BF = anovatest_res$BF, PMPa = anovatest_res$PMPa, PMPb = anovatest_res$PMPb,
                  estimate_res = anovafm)

  cl<-match.call()
  class(anova_res)<-"Bain"
  anova_res$call<-cl
  return(invisible(anova_res))

}


