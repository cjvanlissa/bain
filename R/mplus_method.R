mplus_bain <- function(x, hypothesis, standardize =TRUE){

  Ng             <- x$summaries$NGroups

  if(grepl("(TWO|THREE)LEVEL", toupper(x$summaries$AnalysisType))){
    if(grepl("TWOLEVEL", toupper(x$summaries$AnalysisType))){
      N_lvls         <- 2
    } else {
      N_lvls         <- 3
    }
  } else {
    N_lvls         <- 1
  }

  if(N_lvls > 1){
    stop(paste("Mplus object contains", N_lvls , "levels. Multilevel structures are not yet integrated in bain."))
  }

  if(Ng == 1) {
    out <- mplus_SG(x, hypothesis, standardize)
  } else {
    # Relatively fast way to check for parameters constrained across groups
    # which does not require getting the full estimates table
    pars_per_group <- matrix(unlist(x$tech1$parameterSpecification), ncol = length(x$tech1$parameterSpecification))
    rs <- rowSums(pars_per_group)
    pars_per_group <- pars_per_group[!(is.na(rs)|rs == 0), ]
    pars_per_group <- sweep(pars_per_group, 1, pars_per_group[,1], `-`)

    if(any(pars_per_group[,-1] == 0)){
      out <- mplus_MG_shared(x, hypothesis, standardize)
      } else {
        out <- mplus_MG_unique(x, hypothesis, standardize)
      }
  }
  class(out) <- "bain_estimate"
  attr(out, "analysisType") <- "Mplus"
  out
}

#x <- mg_ex
#tmp <- mplus_estimates_only(x)
#colSums(table(tmp$Group, tmp$id))
#tmp$estimate


mplus_SG <- function(x, hypothesis, standardize ){
  browser()
  estimate <- mplus_estimates_only(x)
  duplicates <- NULL
  if(any(duplicated(estimate$id))){
    dups <- estimate[duplicated(estimate$id), ]
    estimate <- estimate[!duplicated(estimate$id), ]
    duplicates <- lapply(unique(dups$id), function(o){dups$label[dups$id == o]})
    names(duplicates) <- estimate$label[match(dups$id, estimate$id)]
  }
  estimate <- estimate[order(estimate$id), ]
  #covv    <- list(covv)
  #class(est) <- "lavaan"

  coefs <- estimate$est
  names(coefs) <- estimate$label

  Sigma <- x$tech3$paramCov[estimate$id, estimate$id]
  Sigma[upper.tri(Sigma)] <- t(Sigma)[upper.tri(Sigma)]

  colnames(Sigma) <- estimate$label
  rownames(Sigma) <- estimate$label

  out <- list(estimate = coefs,
              Sigma = Sigma,
              N = x$summaries$Observations)
  if(!is.null(duplicates)) out[["label_synonyms"]] <- duplicates
  attr(out, "analysisGroups") <- "single"
  out
  #group_parameters = length(est),
  #joint_parameters = 0)
}




estis <- function(lavobject,standardize){
  browser()
  ## Possibility, always ignoring a specific parameter type (deemed non neccesary)
  # ignores <- "theta"
  free <- extracter(lavobject)

  if( standardize == T){
    #est             <- free$est.std[free$OGmatrix!=ignores& free$lhs!=free$rhs ]
    est              <- free$est.std[free$lhs!=free$rhs] # to ignore variances
  }

  if( standardize == F){
    # est             <- free$est[free$OGmatrix!=ignores& free$lhs!=free$rhs ]
    est              <- free$est[free$lhs!=free$rhs]
  }
  ## Function lavaan uses to determine parameterlabels
  #names(est)       <- lavaan:::getParameterLabels(partable = free[free$OGmatrix!=ignores& free$lhs!=free$rhs ,])
  names(est)        <- lavaan:::getParameterLabels(partable = free[free$lhs!=free$rhs,])

  return(est)
}


vcovis <- function(lavobject, standardize){
  browser()
  est <- estis(lavobject, standardize)

  if( standardize == T){
    covv <- lavInspect(lavobject, "vcov.std.all")
  }

  if( standardize == F){
    covv <- lavInspect(lavobject, "vcov")
  }

  covv <- covv[names(est), names(est)]
  return(covv)
}




Multigrouper <- function(lavobject, standardize){
  browser()
  est         <- estis( lavobject, standardize)
  covv        <- vcovis(lavobject, standardize)
  labls       <- lavInspect(lavobject, "group.label")
  Ng          <- lavInspect(lavobject, what = "ngroups")

  custompara     <- parametertable(lavobject)$label

  ## Number parameters in each group
  ngp         <- length(est)/Ng

  ## How many labels suffices we need to fit behind parameters
  suffix      <- rep(labls,each = ngp)
  ## skipping this for the custom named parameters
  ind         <- grep(pattern = paste0("^", unique(custompara[nchar(custompara)>0]),
                                       collapse = "|^"),
                      x = names(est), invert = T, perl = T, value = F)

  ## Removing the .g lavaan uses
  names(est )  <- gsub(pattern     = "\\.g[[:digit:]]$", x =    names(est ),
                       replacement = "", perl = T)
  rownames(covv)  <- gsub(pattern     = "\\.g[[:digit:]]$", x = rownames(covv),
                          replacement = "", perl = T)
  colnames(covv)  <- gsub(pattern     = "\\.g[[:digit:]]$", x = colnames(covv),
                          replacement = "", perl = T)

  if (any(nchar(custompara)>0)) {
    names(est )[ind]     <- paste0(   names(est )[ind], "." , suffix[ind])
    rownames(covv)[ind]     <- paste0(rownames(covv)[ind], "." , suffix[ind])
    colnames(covv)[ind]     <- paste0(colnames(covv)[ind], "." , suffix[ind])
  }else{
    names(est )          <- paste0(   names(est )     , "." , suffix     )
    rownames(covv)          <- paste0(rownames(covv)     , "." , suffix     )
    colnames(covv)          <- paste0(colnames(covv)     , "." , suffix     )
  }

  return(list(est = est, covv = covv))
}



Multishared <- function(lavobject, standardize){
  browser()
  Ng_n           <- lavInspect(lavobject, what = "nobs"  )
  N              <- lavInspect(lavobject, what = "ntotal")

  X <- Multigrouper(lavobject, standardize)

  if(sum(diff(Ng_n)) != 0){   # Warning is given when the groups size is not perfectly equal.
    warning(paste0(
      "Since at least some parameter values are constrained to be equal across groups,
      a one group method is used. This method is only valid if group sizes are roughly equal.
      The sample size per group are ", paste0( Ng_n ,collapse = " and ") ),
      call. = F,noBreaks.=T)}

  ## Removing the duplicate parameters that are consistent across groups
  X$est        <-      X$est [unique(   names(X$est ))]
  X$covv       <- list(X$covv[unique(colnames(X$covv)),
                              unique(rownames(X$covv))])

  output      <- list("EST"       = X$est,
                      "COVA"      = X$covv,
                      "N"         = N,
                      "grouppara" = length(X$est),
                      "jointpara" = 0)
  return(output)
}

mplus_MG_shared <- function(lavobject, hypothesis, standardize){
  browser()
  MS <-  Multishared(lavobject = lavobject, standardize)
  class(MS$EST) <- "lavaan"
  return(bain(hypothesis       = hypothesis  ,
              n                = MS$N        ,
              Sigma            = MS$COVA     ,
              x                = MS$EST      ,
              group_parameters = MS$grouppara,
              joint_parameters = MS$jointpara))

}

covvsplitter <- function(lavobject, standardize){
  browser()
  X              <- Multigrouper(lavobject, standardize     )
  Ng             <- lavInspect(  lavobject, what = "ngroups")
  Ng_n           <- lavInspect(  lavobject, what = "nobs"   )
  Splitter       <- split( 1:nrow(X$covv),   ceiling(seq_along(1:nrow(X$covv))/(nrow(X$covv)/Ng)))
  covvm          <- lapply(1:Ng, function(Y) as.matrix(X$covv[Splitter[[Y]], Splitter[[Y]]]))
  output         <- list("EST"       = X$est,
                         "COVA"      = covvm,
                         "N"         = Ng_n ,
                         "grouppara" = length(X$est)/Ng,
                         "jointpara" = 0)
  return(output)
}

mplus_MG_unique <- function(lavobject, hypothesis, standardize){
  browser()
  spl <- covvsplitter(lavobject, standardize)
  class(spl$EST) <- "lavaan"
  return(bain(hypothesis       = hypothesis   ,
              n                = spl$N        ,
              Sigma            = spl$COVA     ,
              x                = spl$EST      ,
              group_parameters = spl$grouppara,
              joint_parameters = spl$jointpara))

}
