extracter <- function(lavobject){
 
  # Credit to Caspar van Lissa
  param_id      <- lavInspect(lavobject, "free", list.by.group = F, drop.list.single.group = T)
  origin_matrix <- unlist(lapply(1:length(param_id), function(x){
    rep(names(param_id)[x], length(param_id[[x]]))
  }))
  
  keeprs       <- lapply(names(param_id), function(x) {
    out <- param_id[[x]] == 0
    if (x %in% c("theta", "psi",  "gamma")) {
      out[upper.tri(out)] <- TRUE
    }
    out
  })
  
  # plabel indicates if it is a parameter or if it is a constraint (do not want constrained line)
  estims            <- cbind(parametertable(lavobject)[nchar(parametertable(lavobject)$plabel)>0,],
                             standardizedsolution(lavobject)[,c("est.std","se")])
  
  ## Only the free parameters or parameters explicitly defined by user.

  estims            <- estims[estims$free>0,]
  estims            <- cbind(estims, "OGmatrix" =origin_matrix[!unlist(keeprs)][unlist(param_id)[!unlist(keeprs)]])
  
  return(estims)
}


estis <- function(lavobject,standardize){
  
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


SG_bain <- function(lavobject, hypothesis, standardize ){
  
  N       <- lavInspect(lavobject, what = "ntotal")  
  est     <- estis( lavobject, standardize)
  covv    <- vcovis(lavobject, standardize)
  covv    <- list(covv)
  class(est) <- "lavaan"
  return(bain(hypothesis       = hypothesis ,
              n                = N          ,
              Sigma            = covv       ,
              x                = est        ,
              group_parameters = length(est), 
              joint_parameters = 0))
}



Multigrouper <- function(lavobject, standardize){
  
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

MG_shared_bain <- function(lavobject, hypothesis, standardize){
  
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

MG_unique_bain <- function(lavobject, hypothesis, standardize){
  
  spl <- covvsplitter(lavobject, standardize)
  class(spl$EST) <- "lavaan"
  return(bain(hypothesis       = hypothesis   ,
              n                = spl$N        ,
              Sigma            = spl$COVA     ,
              x                = spl$EST      ,
              group_parameters = spl$grouppara, 
              joint_parameters = spl$jointpara))
  
}


lav_in_bain <- function(lavobject, hypothesis, standardize =T){ 
  require(bain  )
  require(lavaan)
  
  Ng             <- lavInspect(lavobject, what = "ngroups")
  N_lvls         <- lavInspect(lavobject, what = "nlevels") 
  
  if(lavobject@Options$multilevel == T){ # Multilevel structure in data: Gives warning since this aspect is not integrated.
    stop(paste("Lavaan object contains", N_lvls , "levels. Multilevel structures are not yet integrated in Lav_in_Bain."))
  }
  
  if (Ng <= 1) {
    SG_bain(lavobject, hypothesis, standardize)
  }else{
  if (Ng > 1 & any(parametertable(lavobject)$op == "==")){ 
    MG_shared_bain(lavobject, hypothesis, standardize)
  }
  else{
    MG_unique_bain(lavobject, hypothesis, standardize)
    }}
  
}
