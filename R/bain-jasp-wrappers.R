# this file contains subsequently the wrappers for t-test, anova, ancova, and regression
# used to connect JASP to the cran version of bain

# =======================================================================
# t-test

bain_ttest_cran<-function(x,y=NULL,nu=0,type=1,paired=FALSE,seed){
  # ONE GROUP
  if(is.null(y)&&!paired){
    tres <- t_test(x)
    args <- list(
      x = tres,
      hypothesis = switch(type,
                          paste0("x=", nu),
                          paste0("x=", nu, "; x>", nu),
                          paste0("x=", nu, "; x<", nu),
                          paste0("x>", nu, "; x<", nu),
                          paste0("x=", nu, "; x>", nu, "; x<", nu))
      )
    set.seed(seed)
    result <- do.call(bain, args)

  }

  # INDEPENDENT SAMPLES
  if(!is.null(y)&&!paired){
    tres <- t_test(x,y,paired = FALSE, var.equal = FALSE)
    set.seed(seed)
    args <- list(
      x = tres,
      hypothesis = switch(type, "x=y", "x=y; x>y", "x=y; x<y", "x>y; x<y", "x=y; x>y; x<y")

    )
    result <- do.call(bain, args)
  }

  # PAIRED SAMPLE
  if(!is.null(y)&&paired){
    tres <- t_test(x,y,paired = TRUE)
    set.seed(seed)
    args <- list(
      x = tres,
      hypothesis = switch(type,
                          "difference=0", "difference=0;difference>0", "difference=0;difference<0", "difference>0;difference<0", "difference=0;difference>0;difference<0"
                          )
    )
    result <- do.call(bain, args)
  }
  return(invisible(result))
}

# ======================================================================
# ANOVA

bain_anova_cran<-function(X,dep,group,hyp,seed){
  # maak een factor van group
  c1 <- paste0("X$",group,"<- as.factor(X$",group,")")
  eval(parse(text = c1))
  # roep lm aan
  args <- list(
    formula = as.formula(paste0(dep, "~", group, "-1")),
    data <- X
  )
  lmres <- do.call(lm, args)

  # construeer hyp als deze als NULL binnenkomt
  if (is.null(hyp)){
    hyp <- names(coef(lmres))
    hyp <- paste0(hyp, collapse = "=")
  }

  # roep bain aan met lmres en hyp als input
  #c3 <- paste0("bain::bain(lmres,","\"",hyp,"\"",")")
  #result <- eval(parse(text = c3))
  args <- list(
    x = lmres,
    hypothesis = hyp
  )
  set.seed(seed)
  result <- do.call(bain, args)

  return(invisible(result))
}

# =================================================================
# ANCOVA

bain_ancova_cran<-function(X,dep,cov,group,hyp,seed){

  # maak een factor van group en tel het aantal groepen
  c1 <- paste0("X$",group,"<- as.factor(X$",group,")")
  eval(parse(text = c1))
  c4 <- paste0("ngroup <- nlevels(X$",group,")")
  eval(parse(text = c4))

  # make an array of cov
  cov <- as.character(strsplit(cov," ")[[1]])
  ncov <- length(cov)
  cov <- paste0(cov,collapse = "+")

  # centreer de covariaten
  for (i in 1:ncov){
    X[,(1+i)] <- X[,(1+i)] - mean(X[,(1+i)])
  }

  # roep lm aan
  args <- list(
    formula = as.formula(paste0(dep, "~", group, "+", cov, "-1")),
    data <- X
  )
  lmres <- do.call(lm, args)

  # construeer hyp als deze als NULL binnenkomt
  if (is.null(hyp)){
    hyp <- names(coef(lmres))
    hyp <- hyp[1:(length(hyp)-ncov)]
    hyp <- paste0(hyp, collapse = "=")
  }

  # roep bain aan met lmres en hyp als input
  args <- list(
    x = lmres,
    hypothesis = hyp
  )
  set.seed(seed)
  do.call(bain, args)
}

# ===============================================================
# multiple regression

bain_regression_cran<-function(X,dep,pred,hyp,std,seed){



  # make an array of pred
  pred <- as.character(strsplit(pred," ")[[1]])
  predforhyp <- pred
  npred <- length(pred)
  pred <- paste0(pred,collapse = "+")

  # roep lm aan
  args <- list(
    formula = as.formula(paste0(dep, "~", pred)),
    data <- X
  )
  lmres <- do.call(lm, args)

  # construeer hyp als deze als NULL binnenkomt
  if (is.null(hyp)){
    hyp <- predforhyp
    hyp <- sapply(hyp,function(x) paste0(x,"=0"))
    hyp <- paste0(hyp, collapse = " & ")
  }

  # roep bain aan met lmres en hyp als input
  args <- list(
    x = lmres,
    hypothesis = hyp,
    standardize = std
  )
  set.seed(seed)
  do.call(bain, args)
}



