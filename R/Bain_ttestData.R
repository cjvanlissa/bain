#' Bayesian t test using approximate adjusted fractional Bayes factors
#'
#' This function computes approximated adjusted fractional Bayes factors
#' one-sample or two-sample t test.
#'
#'
#' @aliases Bain_ttestData Bain_ttestData
#' @param x A vector for the sample
#' @param y A vector for the sample in the second group (optional).
#' @param nu A numeric number for the null value in t test. Default number is
#' nu = 0.
#' @param type An integer number (1-4) indicating the type of the t test.  "1"
#' means theta = 0 vs theta != 0.  "2" means theta = 0 vs theta > 0.  "3" means
#' theta = 0 vs theta < 0.  "4" means theta > 0 vs theta < 0.  "5" means theta
#' = 0 vs theta > 0 vs theta < 0.  "theta" is the mean parameter of the sample.
#' @param paired Paired = TRUE for paired t test. Default is paried = FALSE.
#' @return %% ~Describe the value returned %% If it is a LIST, use Return a
#' list that contains Bayes factor and posterior model probabilities.
#'
#' %% ...
#' @author Xin Gu, Herbert Hoijtink, Joris Mulder
#' @keywords internal htest
#' @examples
#' \dontrun{
#'   #Example 1
#'   #One sample t test:
#'   #H0: theta=0
#'   x<-c(-1,1,2,-1,-0.5)
#'   res<-Bain_ttestData(x)
#'
#'   plot(res)
#'   #Results for PMPs are plotted.
#'
#'   #Other types
#'   Bain_ttestData(x,type=2)
#'   Bain_ttestData(x,type=3)
#'   Bain_ttestData(x,type=4)
#'   Bain_ttestData(x,type=5)
#'
#'
#'   #Example2
#'   #Two sample t test:
#'   #H0: theta1=theta2
#'   x<-c(-1,1,2,-1,-0.5)
#'   y<-c(0,1,2,0,1)
#'   res<-Bain_ttestData(x,y)
#'
#'   plot(res)
#'   #Results for PMPs are plotted.
#'
#'
#'   #Example3
#'   #paired t test:
#'   #H0: theta1=theta2
#'   x<-c(-1,1,2,-1,-0.5)
#'   y<-c(0,1,2,0,1)
#'   res<-Bain_ttestData(x,y,paired=TRUE)
#'
#'   plot(res)
#'   #Results for PMPs are plotted.
#' }
Bain_ttestData<-function(x,y=NULL,nu=0,type=1,paired=FALSE){
  if(is.null(y)&&!paired){
    estimate<-mean(x)
    n<-length(x)
    variance<-(sd(x))^2/n
    ttDatares<-Bain_ttest(estimate=estimate,variance=variance,n=n,nu=nu,type=type)
  }else if(is.null(y)&&paired){stop("paired test requires two groups")
  }else if(!is.null(y)&&!paired){
    estimate<-c(mean(x),mean(y))
    n1<-length(x)
    n2<-length(y)
    variance<-c((sd(x))^2/n1,(sd(y))^2/n2)
    ttDatares<-Bain_ttest(estimate=estimate,variance=variance,n=c(n1,n2),nu=nu,type=type)
  }else if(!is.null(y)&&paired){
    if(length(x)!=length(y)){stop("paired test requires equal size of two groups")}
    df<-x-y
    estimate<-mean(df)
    n<-length(df)
    variance<-(sd(df))^2/n
    ttDatares<-Bain_ttest(estimate=estimate,variance=variance,n=n,nu=nu,type=type)
  }

  cl<-match.call()
  class(ttDatares)<-"Bain"
  ttDatares$call<-cl
  return(invisible(ttDatares))
}


