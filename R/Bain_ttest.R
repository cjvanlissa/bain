#t test
#one sample t test



#' Bayesian t test using approximate adjusted fractional Bayes factors
#'
#' This function computes approximated adjusted fractional Bayes factors for
#' one-sample or two-sample t test.
#'
#'
#' @aliases Bain_ttest Bain_ttest
#' @param estimate A number (one-sample t test) or a vector (two-sample t test)
#' giving the estimate(s) of the group mean(s).
#' @param variance A number (one-sample t test) or a vector (two-sample t test)
#' giving the variance(s) of the group mean(s).
#' @param n A positive integer number (one-sample t test) or vector (two-sample
#' t test) indicating the sample size in the data.
#' @param nu A numeric number for the null value in t test. Default number is
#' nu = 0.
#' @param type An integer number (1-4) indicating the type of the t test. "1"
#' means H0: mu = 0 vs Hu: mu != 0. "2" means H0: mu = 0 vs H1: mu > 0. "3"
#' means H0: mu = 0 vs H2: mu < 0. "4" means H1: mu > 0 vs H2: mu < 0. "5"
#' means H0: mu = 0 vs H1: mu > 0 vs H2: mu < 0.
#' @return %% ~Describe the value returned %% If it is a LIST, use Return a
#' list that contains Bayes factor and posterior model probability for each
#' hypothesis.
#'
#' %% ...
#' @author Xin Gu, Herbert Hoijtink, Joris Mulder
#' @keywords internal htest
#' @examples
#' \dontrun{
#' #Example 1
#' #One sample t test:
#' #H0: mu=0
#' #Input
#' estimate<-0.5    #Estimate of theta
#' variance<-1    #Variance of theta
#' n<-20      #samplesize
#' res<-Bain_ttest(estimate,variance,n,nu=0,type=1) #Run
#'
#' #Output printed
#' #Hypotheses H0: mu = 0 vs Hu: mu != 0
#' #
#' #t test result
#' # BF_0u   PMP_0   PMP_u
#' # 3.947   0.798   0.202
#'
#' plot(res)
#' #Results for PMPs are plotted.
#'
#' #Other types
#' Bain_ttest(estimate,variance,n,nu=0,type=2) #Run
#' Bain_ttest(estimate,variance,n,nu=0,type=3) #Run
#' Bain_ttest(estimate,variance,n,nu=0,type=4) #Run
#' Bain_ttest(estimate,variance,n,nu=0,type=5) #Run
#'
#'
#' #Example2
#' #Two sample t test:
#' #H0: mu1=mu2
#' #Input
#' estimate<-c(0.2,0.5)
#' variance<-c(0.01,0.02)
#' n<-c(20,40)
#' res<-Bain_ttest(estimate,variance,nu=0,n) #Run
#'
#' #Output printed
#' #Hypotheses H0: mu1 = mu2 vs Hu: mu1 != mu2
#'
#' #t test result
#' # BF_0u   PMP_0   PMP_u
#' # 1.822   0.646   0.354
#'
#' plot(res)
#' #Results for PMPs are plotted.
#' }
Bain_ttest<-function(estimate,variance,n,nu=0,type=1){
  if(type==1&&length(estimate)==1){  # mu=nu vs mu!=nu
    Bain_res<-Bain(estimate=estimate,
                   Sigma=variance,
                   grouppara=0,
                   jointpara=1,
                   n=n,
                   ERr=matrix(c(1,nu),1,2),
                   seed=100,print=FALSE)

    res<-Bain_res$testResult["H1",c("BF","PMPb")]
    res<-cbind(res,(1-res[2]))
    names(res)<-c("BF_0u","PMP_0","PMP_u")
    cat(paste("Hypotheses H0: mu = ",nu," vs Hu: mu != ",nu,sep=""),sep="\n")
  }

  if(type==2&&length(estimate)==1){ # mu=nu vs mu>nu
    Bain_res<-Bain(estimate=estimate,
                   Sigma=variance,
                   grouppara=0,
                   jointpara=1,
                   n=n,
                   matrix(c(1,nu),1,2),
                   matrix(0,0,0),
                   matrix(0,0,0),
                   matrix(c(1,nu),1,2),
                   seed=100,print=FALSE)

    fit1<-Bain_res$testResult["H1","fit"]
    fit2<-Bain_res$testResult["H2","fit"]
    com1<-Bain_res$testResult["H1","complexity"]
    com2<-Bain_res$testResult["H2","complexity"]
    #BF1<-Bain_res$testResult["H1","BF"]
    #BF2<-Bain_res$testResult["H2","BF"]
    BF<-fit1/com1/(fit2/com2)
    PMP1<-fit1/com1/(fit1/com1+fit2/com2)
    PMP2<-1-PMP1

    res<-t(c(BF,PMP1,PMP2))
    colnames(res)<-c("BF_01","PMP_0","PMP_1")
    cat(paste("Hypotheses H0: mu = ",nu," vs H1: mu > ",nu,sep=""),sep="\n")

  }

  if(type==3&&length(estimate)==1){# mu=nu vs mu<nu
    Bain_res<-Bain(estimate=estimate,
                   Sigma=variance,
                   grouppara=0,
                   jointpara=1,
                   n=n,
                   matrix(c(1,nu),1,2),
                   matrix(0,0,0),
                   matrix(0,0,0),
                   matrix(c(-1,-nu),1,2),
                   seed=100,print=FALSE)

    fit1<-Bain_res$testResult["H1","fit"]
    fit2<-Bain_res$testResult["H2","fit"]
    com1<-Bain_res$testResult["H1","complexity"]
    com2<-Bain_res$testResult["H2","complexity"]
    #BF1<-Bain_res$testResult["H1","BF"]
    #BF2<-Bain_res$testResult["H2","BF"]
    BF<-fit1/com1/(fit2/com2)
    PMP1<-fit1/com1/(fit1/com1+fit2/com2)
    PMP2<-1-PMP1

    res<-t(c(BF,PMP1,PMP2))
    colnames(res)<-c("BF_01","PMP_0","PMP_1")
    cat(paste("Hypotheses H0: mu = ",nu," vs H1: mu < ",nu,sep=""),sep="\n")

  }

  if(type==4&&length(estimate)==1){# mu>nu vs mu<nu
    Bain_res<-Bain(estimate=estimate,
                   Sigma=variance,
                   grouppara=0,
                   jointpara=1,
                   n=n,
                   matrix(0,0,0),
                   matrix(c(1,nu),1,2),
                   matrix(0,0,0),
                   matrix(c(-1,-nu),1,2),
                   seed=100,print=FALSE)

    fit1<-Bain_res$testResult["H1","fit"]
    fit2<-Bain_res$testResult["H2","fit"]
    com1<-Bain_res$testResult["H1","complexity"]
    com2<-Bain_res$testResult["H2","complexity"]
    #BF1<-Bain_res$testResult["H1","BF"]
    #BF2<-Bain_res$testResult["H2","BF"]
    BF<-fit1/com1/(fit2/com2)
    PMP1<-fit1/com1/(fit1/com1+fit2/com2)
    PMP2<-1-PMP1

    res<-t(c(BF,PMP1,PMP2))
    colnames(res)<-c("BF_12","PMP_1","PMP_2")
    cat(paste("Hypotheses H1: mu > ",nu," vs H2: mu < ",nu,sep=""),sep="\n")
  }

  if(type==5&&length(estimate)==1){# mu=nu vs mu>nu vs mu<nu
    Bain_res<-Bain(estimate=estimate,
                   Sigma=variance,
                   grouppara=0,
                   jointpara=1,
                   n=n,
                   matrix(c(1,nu),1,2),
                   matrix(0,0,0),
                   matrix(0,0,0),
                   matrix(c(1,nu),1,2),
                   matrix(0,0,0),
                   matrix(c(-1,-nu),1,2),
                   seed=100,print=FALSE)

    fit1<-Bain_res$testResult["H1","fit"]
    fit2<-Bain_res$testResult["H2","fit"]
    fit3<-Bain_res$testResult["H3","fit"]
    com1<-Bain_res$testResult["H1","complexity"]
    com2<-Bain_res$testResult["H2","complexity"]
    com3<-Bain_res$testResult["H3","complexity"]

    BF12<-fit1/com1/(fit2/com2)
    BF13<-fit1/com1/(fit3/com3)
    BF23<-fit2/com2/(fit3/com3)
    PMP1<-fit1/com1/(fit1/com1+fit2/com2+fit3/com3)
    PMP2<-fit2/com2/(fit1/com1+fit2/com2+fit3/com3)
    PMP3<-fit3/com3/(fit1/com1+fit2/com2+fit3/com3)

    res<-t(c(BF12,BF13,BF23,PMP1,PMP2,PMP3))
    colnames(res)<-c("BF_01","BF_02","BF_12","PMP_0","PMP_1","PMP_2")
    cat(paste("Hypotheses H0: mu = ",nu," vs H1: mu > ",nu," vs H2: mu < ",nu,sep=""),sep="\n")
  }


  if(type==1&&length(estimate)==2){  # mu1=mu2 vs mu1!=mu2   or mu1-mu2=nu
    Bain_res<-Bain(estimate=estimate,
                   Sigma=list(as.matrix(variance[1]),as.matrix(variance[2])),
                   grouppara=1,
                   jointpara=0,
                   n=n,
                   ERr=matrix(c(1,-1,nu),1,3),
                   seed=100,print=FALSE)

    res<-Bain_res$testResult["H1",c("BF","PMPb")]
    res<-cbind(res,(1-res[2]))
    names(res)<-c("BF_0u","PMP_0","PMP_u")
    cat("Hypotheses H0: mu1 = mu2 vs Hu: mu1 != mu2",sep="\n")

  }

  if(type==2&&length(estimate)==2){ # mu1=mu2 vs mu1>mu2
    Bain_res<-Bain(estimate=estimate,
                   Sigma=list(as.matrix(variance[1]),as.matrix(variance[2])),
                   grouppara=1,
                   jointpara=0,
                   n=n,
                   matrix(c(1,-1,nu),1,3),
                   matrix(0,0,0),
                   matrix(0,0,0),
                   matrix(c(1,-1,nu),1,3),
                   seed=100,print=FALSE)

    fit1<-Bain_res$testResult["H1","fit"]
    fit2<-Bain_res$testResult["H2","fit"]
    com1<-Bain_res$testResult["H1","complexity"]
    com2<-Bain_res$testResult["H2","complexity"]
    #BF1<-Bain_res$testResult["H1","BF"]
    #BF2<-Bain_res$testResult["H2","BF"]
    BF<-fit1/com1/(fit2/com2)
    PMP1<-fit1/com1/(fit1/com1+fit2/com2)
    PMP2<-1-PMP1

    res<-t(c(BF,PMP1,PMP2))
    colnames(res)<-c("BF_01","PMP_0","PMP_1")
    cat("Hypotheses H0: mu1 = mu2 vs H1: mu1 > mu2",sep="\n")

  }

  if(type==3&&length(estimate)==2){# mu1=mu2 vs mu1<mu2
    Bain_res<-Bain(estimate=estimate,
                   Sigma=list(as.matrix(variance[1]),as.matrix(variance[2])),
                   grouppara=1,
                   jointpara=0,
                   n=n,
                   matrix(c(1,-1,nu),1,3),
                   matrix(0,0,0),
                   matrix(0,0,0),
                   matrix(c(-1,1,-nu),1,3),
                   seed=100,print=FALSE)

    fit1<-Bain_res$testResult["H1","fit"]
    fit2<-Bain_res$testResult["H2","fit"]
    com1<-Bain_res$testResult["H1","complexity"]
    com2<-Bain_res$testResult["H2","complexity"]
    #BF1<-Bain_res$testResult["H1","BF"]
    #BF2<-Bain_res$testResult["H2","BF"]
    BF<-fit1/com1/(fit2/com2)
    PMP1<-fit1/com1/(fit1/com1+fit2/com2)
    PMP2<-1-PMP1

    res<-t(c(BF,PMP1,PMP2))
    colnames(res)<-c("BF_01","PMP_0","PMP_1")

    cat("Hypotheses H0: mu1 = mu2 vs H1: mu1 < mu2",sep="\n")

  }

  if(type==4&&length(estimate)==2){# mu1>mu2 vs mu1<mu2
    Bain_res<-Bain(estimate=estimate,
                   Sigma=list(as.matrix(variance[1]),as.matrix(variance[2])),
                   grouppara=1,
                   jointpara=0,
                   n=n,
                   matrix(0,0,0),
                   matrix(c(1,-1,nu),1,3),
                   matrix(0,0,0),
                   matrix(c(-1,1,-nu),1,3),
                   seed=100,print=FALSE)

    fit1<-Bain_res$testResult["H1","fit"]
    fit2<-Bain_res$testResult["H2","fit"]
    com1<-Bain_res$testResult["H1","complexity"]
    com2<-Bain_res$testResult["H2","complexity"]
    #BF1<-Bain_res$testResult["H1","BF"]
    #BF2<-Bain_res$testResult["H2","BF"]
    BF<-fit1/com1/(fit2/com2)
    PMP1<-fit1/com1/(fit1/com1+fit2/com2)
    PMP2<-1-PMP1

    res<-t(c(BF,PMP1,PMP2))
    colnames(res)<-c("BF_12","PMP_1","PMP_2")

    cat("Hypotheses H1: mu1 > mu2 vs H2: mu1 < mu2",sep="\n")
  }

  if(type==5&&length(estimate)==2){# mu1=mu2 vs mu1>mu2 vs mu1<mu2
    Bain_res<-Bain(estimate=estimate,
                   Sigma=list(as.matrix(variance[1]),as.matrix(variance[2])),
                   grouppara=1,
                   jointpara=0,
                   n=n,
                   matrix(c(1,-1,nu),1,3),
                   matrix(0,0,0),
                   matrix(0,0,0),
                   matrix(c(1,-1,nu),1,3),
                   matrix(0,0,0),
                   matrix(c(-1,1,-nu),1,3),
                   seed=100,print=FALSE)

    fit1<-Bain_res$testResult["H1","fit"]
    fit2<-Bain_res$testResult["H2","fit"]
    fit3<-Bain_res$testResult["H3","fit"]
    com1<-Bain_res$testResult["H1","complexity"]
    com2<-Bain_res$testResult["H2","complexity"]
    com3<-Bain_res$testResult["H3","complexity"]
    BF12<-fit1/com1/(fit2/com2)
    BF13<-fit1/com1/(fit3/com3)
    BF23<-fit2/com2/(fit3/com3)
    PMP1<-fit1/com1/(fit1/com1+fit2/com2+fit3/com3)
    PMP2<-fit2/com2/(fit1/com1+fit2/com2+fit3/com3)
    PMP3<-fit3/com3/(fit1/com1+fit2/com2+fit3/com3)

    res<-t(c(BF12,BF13,BF23,PMP1,PMP2,PMP3))
    colnames(res)<-c("BF_01","BF_02","BF_12","PMP_0","PMP_1","PMP_2")
    cat("Hypotheses H0: mu1 = mu2 vs H1: mu1 > mu2 vs H2: mu1 < mu2" ,sep="\n")

  }


  res.x<-data.frame(formatC(as.matrix(res),digits = 3, format = "f"))
  rownames(res.x)<-rownames(res)<-""

  cl<-match.call()

  writeLines(" ")
  cat("t test result", sep="\n")
  write.table(capture.output(res.x),col.names = FALSE,row.names = FALSE,quote = FALSE)

  ttest_res<-as.data.frame(res)
  class(ttest_res)<-"Bain"
  ttest_res$call<-cl
  return(invisible(ttest_res))
}




