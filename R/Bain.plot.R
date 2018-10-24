#' @method plot Bain
#' @export
plot.Bain<-function(x,...){
  fun_type<-as.character(x$call)[1]

  ##plot for Bain
  if(fun_type=="Bain"){
    PMPa<-x$testResult$PMPa
    PMPb<-c(x$testResult$PMPb,1-sum(x$testResult$PMPb))

    numH<-length(as.matrix(x$testResult))/5
    P_lables<-paste("H",1:numH ,sep="")

    if(numH==1){
      par(mfrow=c(1,1))
      pie(PMPb,labels=c("H1","Hu"),main="PMP",col=c("green","white"))
    }
    if(numH>1){
      par(mfrow=c(1,2))
      pie(PMPa,labels=P_lables,main="PMP excluding Hu",col=3:(length(P_lables)+2))
      pie(PMPb,labels=c(P_lables,"Hu"),main="PMP including Hu",col=c(3:(length(P_lables)+2),"white"))
    }
  }

  ##plot for t test
  if(fun_type=="Bain_ttest"||fun_type=="Bain_ttestData"){
    if(length(x)==4&&names(x)[1]=="BF_0u"){
      PMP<-x$PMP_0
      par(mfrow=c(1,1))
      pie(c(PMP,1-PMP),labels=c("H0","Hu"),main="PMP",col=c("green","white"))
    }

    if(length(x)==4&&names(x)[1]=="BF_01"){
      PMP<-x$PMP_0
      par(mfrow=c(1,1))
      pie(c(PMP,1-PMP),labels=c("H0","H1"),main="PMP",col=c("green","white"))
    }

    if(length(x)==4&&names(x)[1]=="BF_12"){
      PMP<-x$PMP_1
      par(mfrow=c(1,1))
      pie(c(PMP,1-PMP),labels=c("H1","H2"),main="PMP",col=c("green","white"))
    }

    #if(length(x)==4&&names(x)[1]=="BF_02"){
    #  PMP<-x$PMP_0
    #  par(mfrow=c(1,1))
    #  pie(c(PMP,1-PMP),labels=c("H0","H2"),main="PMP",col=c("green","white"))
    #}

    if(length(x)==7){
      PMP<-c(x$PMP_0,x$PMP_1,x$PMP_2)
      par(mfrow=c(1,1))
      pie(PMP,labels=c("H0","H1","H2"),main="PMP",col=c("green","white","yellow"))
    }
  }

  ##for anova, ancova and regression
  if(fun_type=="Bain_anova"||fun_type=="Bain_ancova"||fun_type=="Bain_regression"
     ||fun_type=="Bain_anova_cm"||fun_type=="Bain_ancova_cm"||fun_type=="Bain_regression_cm"){
    PMPa<-x$PMPa
    PMPb<-c(x$PMPb,1-sum(x$PMPb))

    numH<-length(x$BF)
    P_lables<-paste("H",1:numH ,sep="")

    if(numH==1){
      par(mfrow=c(1,1))
      pie(PMPb,labels=c("H1","Hu"),main="PMP",col=c("green","white"))
    }
    if(numH>1){
      par(mfrow=c(1,2))
      pie(PMPa,labels=P_lables,main="PMP excluding Hu",col=3:(length(P_lables)+2))
      pie(PMPb,labels=c(P_lables,"Hu"),main="PMP including Hu",col=c(3:(length(P_lables)+2),"white"))
    }
  }

}








