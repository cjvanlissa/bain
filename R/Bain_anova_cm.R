#' Bayes factors of informative hypotheses for ANOVA models (user-friendly
#' hypotheses input)
#'
#' This function computes approximated adjusted fractional Bayes factors for
#' anova test with user-friendly hypotheses input.
#'
#'
#' @aliases Bain_anova_cm Bain_anova_cm
#' @param X A data frame that contains dependent variable and group factor.
#' @param dep_var A character that indicates the name of the dependent variable
#' in X.
#' @param group A character that indicates the name of the group factor in X.
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
#' \dontrun{
#'   #Example 1
#'   #Hypotheses
#'   #H1: group.1=group.2=group.3   #group means
#'   #H2: group.1>group.2>group.3
#'   #H3: group.1<group.2<group.3
#'
#'   #Input
#'   X<-data.frame(c(rnorm(20,0,1),rnorm(40,.5,2),rnorm(60,1,5)),
#'                 c(rep(1,20),rep(2,40),rep(3,60)))
#'   names(X)<-c("dep_var","group")
#'   head(X)
#'
#'   hyp<-"group.1=group.2=group.3;
#'         group.1>group.2>group.3;
#'         group.1<group.2<group.3"
#'
#'   res<-Bain_anova_cm(X,"dep_var","group",hyp) #run
#'   #Results are printed.
#'   #Results for fit, complexity, Bayes factor, and PMPs are saved in "res":
#'
#'   plot(res)
#'   #Results for PMPs are plotted.
#'
#'   #Example 2
#'   #Hypotheses
#'
#'   #Input
#'   df<-data.frame(c(rbinom(20,10,0.5),rbinom(40,10,0.6),rbinom(30,10,0.6),rbinom(30,10,0.7)),
#'                 c(rep("A",20),rep("B",40),rep("AB",30),rep("O",30)))
#'   names(df)<-c("ambition","bloodtype")
#'   head(df)
#'
#'   hyp<-"bloodtype.O = bloodtype.AB = bloodtype.A = bloodtype.B;
#'         bloodtype.O > bloodtype.AB > bloodtype.A = bloodtype.B"
#'
#'   res<-Bain_anova_cm(df,"ambition","bloodtype",hyp) #run
#'   #Results are printed.
#'   #Results for fit, complexity, Bayes factor, and PMPs are saved in "res":
#'
#'   plot(res)
#'   #Results for PMPs are plotted.
#' }
#' @export
Bain_anova_cm<-function(X, dep_var=NULL, group=NULL, hyp){

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
    group_name<-group
  }else if(is.null(group)){
    groupf <- factor(X[,2])
    group_name<-names(X)[2]
  }else{
    stop("group should be a character or NULL")
  }

  n<-unlist(lapply(split(depv,groupf),length))

  anovafm <-  lm(depv ~ groupf -1)
  estimate<-coef(anovafm)
  variance <- (summary(anovafm)$sigma)**2
  variance <- 1/n * variance
  covlist<- lapply(as.list(variance),matrix) ##convert variances to list of variances used in Bain

  ##possible variable names
  varnames_g<-paste("g",1:length(n),sep = "")
  varnames_gd<-paste("g",1:length(n),sep = ".")
  varnames_gn<-paste(group_name,1:length(n),sep = ".")
  varnames_gf<-paste(group_name,levels(groupf),sep = ".")

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
  IR_chara<-paste0("TRr[[",1:length(TRr),"]]",sep=",",collapse = "")

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


