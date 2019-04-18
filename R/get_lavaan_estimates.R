#' @importFrom utils getFromNamespace
getParameterLabels <-
  getFromNamespace("getParameterLabels", "lavaan")

#' @importFrom lavaan lavInspect parametertable standardizedsolution

free_param_extracter <- function(x) {

  ## Code not needed now: Used to add information about which type of variance, mean, or intercept the parameter indicates

  #param_id      <- lavInspect(x,"free",
  #                            list.by.group = FALSE,
  #                            drop.list.single.group = TRUE)
  #origin_matrix <- unlist(lapply(1:length(param_id), function(x) {
  #  rep(names(param_id)[x], length(param_id[[x]]))
  #}))
  # keeprs       <- lapply(names(param_id), function(x) {
  #   out <- param_id[[x]] == 0
  #   if (x %in% c("theta", "psi",  "gamma")) {
  #     out[upper.tri(out)] <- TRUE
  #   }
  #   out
  # })

  # plabel indicates if it is a parameter or if there are constraints between parameters (do not want constrained info here)
  estims            <-
    cbind(parametertable(x)[nchar(parametertable(x)$plabel) >
                                      0,],
          standardizedsolution(x)[, c("est.std", "se")])

  ## Only the free parameters or parameters explicitly defined by user.

  estims            <- estims[estims$free > 0, ]

  ## Indication from which lavaan matrix originates (psi, gamma, etc..)
  # estims  <- cbind(estims, "OGmatrix" = origin_matrix[!unlist(keeprs)][unlist(param_id)[!unlist(keeprs)]])

  estims
}


estims_lavaan <- function(x, standardize) {

   free <- free_param_extracter(x)

  if (standardize == TRUE) {
    estims   <-   free$est.std[free$lhs != free$rhs] # to ignore variances
  }

  if (standardize == FALSE) {
    estims   <- free$est[free$lhs != free$rhs]
  }
  ## Function lavaan uses to determine parameterlabels. This function creates a dependency
  names(estims)     <- getParameterLabels(partable = free[free$lhs != free$rhs, ])

  estims
}


vcovs_lavaan <- function(x, standardize) {

  estims <- estims_lavaan(x, standardize)

  if (standardize == TRUE) {                      # Most situations std should be used over unstand.
    covv <- lavInspect(x, "vcov.std.all")
  }

  if (standardize == FALSE) {
    covv <- lavInspect(x, "vcov")
  }

  covv <- covv[names(estims), names(estims)]

  covv
}


Single_group_estimates_lavaan <- function(x, standardize) {

  N          <- lavInspect(x, what = "ntotal")
  estims     <- estims_lavaan(x, standardize)
  covv       <- vcovs_lavaan(x, standardize)
  covv       <- list(covv)


    list('n'              = N          ,
      'Sigma'            = covv       ,
      'x'                = estims        ,
      'group_parameters' = length(estims),
      'joint_parameters' = 0)

}



MultiGroup_lavaan_extract <- function(x, standardize) {

  estims      <- estims_lavaan(x, standardize)
  covv        <- vcovs_lavaan(x, standardize)
  labls       <- lavInspect(x, what =  "group.label")
  Ng          <- lavInspect(x, what =  "ngroups")

  # user labelled parameters
  custompara     <- parametertable(x)$label

  ## Number parameters in each group
  ngp         <- length(estims) / Ng

  ## How many labels suffices we need to fit behind parameters
  suffix      <- rep(labls, each = ngp)
  ## skipping this for the custom named parameters
  ind         <-
    grep(
      pattern = paste0("^", unique(custompara[nchar(custompara) > 0]),
                       collapse = "|^"),
      x = names(estims),
      invert = T,
      perl = T,
      value = F
    )

  ## Removing the .g lavaan uses
  names(estims)  <-
    gsub(
      pattern     = "\\.g[[:digit:]]$",
      x =    names(estims),
      replacement = "",
      perl = T
    )
  rownames(covv)  <-
    gsub(
      pattern     = "\\.g[[:digit:]]$",
      x = rownames(covv),
      replacement = "",
      perl = T
    )
  colnames(covv)  <-
    gsub(
      pattern     = "\\.g[[:digit:]]$",
      x = colnames(covv),
      replacement = "",
      perl = T
    )

  if (any(nchar(custompara) > 0)) {
    names(estims)[ind]     <-
      paste0(names(estims)[ind], "." , suffix[ind])
    rownames(covv)[ind]     <-
      paste0(rownames(covv)[ind], "." , suffix[ind])
    colnames(covv)[ind]     <-
      paste0(colnames(covv)[ind], "." , suffix[ind])
  } else{
    names(estims)          <-
      paste0(names(estims)     , "." , suffix)
    rownames(covv)          <-
      paste0(rownames(covv)     , "." , suffix)
    colnames(covv)          <-
      paste0(colnames(covv)     , "." , suffix)
  }

  list('estims' = estims, 'covv' = covv)
}



Mult_Group_common_param_lavaan <- function(x, standardize) {
  Ng_n           <- lavInspect(x, what = "nobs")
  N              <- lavInspect(x, what = "ntotal")

  X <- MultiGroup_lavaan_extract(x, standardize)

  if (sum(diff(Ng_n)) != 0) {
    # Warning is given when the groups size is not perfectly equal.
    warning(
      paste0(
        "Since at least some parameter values are constrained to be equal across groups,
        a one group method is used. This method is only valid if group sizes are roughly equal.
        The sample size per group are ",
        paste0(Ng_n , collapse = " and ")
      ),
      call. = FALSE,
      noBreaks. = TRUE
    )
  }

  ## Removing the duplicate parameters that are consistent across groups
  X$estims        <-      X$estims [unique(names(X$estims))]
  X$covv       <- list(X$covv[unique(colnames(X$covv)),
                              unique(rownames(X$covv))])

list("x"         = X$estims,
    "Sigma"      = X$covv,
    "n"          = N,
    "group_parameters" = length(X$estims),
    "joint_parameters" = 0)
}


Mult_Group_no_common_param_lavaan <- function(x, standardize) {

  X              <- MultiGroup_lavaan_extract(x, standardize)
  Ng             <- lavInspect(x, what = "ngroups")
  Ng_n           <- lavInspect(x, what = "nobs")

  Splitter       <-
    split(1:nrow(X$covv),   ceiling(seq_along(1:nrow(X$covv)) / (nrow(X$covv) /
                                                                   Ng)))
  covvm          <-
    lapply(1:Ng, function(Y)
      as.matrix(X$covv[Splitter[[Y]], Splitter[[Y]]]))

list("x"       = X$estims,
    "Sigma"      = covvm,
    "n"         = Ng_n ,
    "group_parameters" = length(X$estims) / Ng,
    "joint_parameters" = 0)

}



