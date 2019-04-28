#' @importFrom utils getFromNamespace
lav_getParameterLabels <-
  getFromNamespace("getParameterLabels", "lavaan")

#' @importFrom lavaan lavInspect parametertable standardizedsolution

lav_get_est <- function(x, standardize) {
  estims <- cbind(parametertable(x)[nchar(parametertable(x)$plabel) > 0, ],
                  standardizedsolution(x)[, c("est.std", "se")])

  ## Only the free parameters or parameters explicitly defined by user.
  estims <- estims[estims$free > 0, ]
  variance_parameters <- estims$lhs == estims$rhs
  estims <- estims[!variance_parameters, ]
  estims$parameter_label <- lav_getParameterLabels(partable = estims)
  estims
}


lav_get_vcov <- function(x, param_labels, standardize) {
  if (standardize) {
    covv <- lavInspect(x, "vcov.std.all")
  } else {
    covv <- lavInspect(x, "vcov")
  }
  covv[param_labels, param_labels]
}



lav_get_estimates_single_group <- function(x, standardize) {
  browser()
  parameter_table <- lav_get_est(x, standardize)
  if (standardize) {
    estims <- parameter_table$est.std
  } else {
    estims <- parameter_table$est
  }
  names(estims) <- parameter_table$parameter_label

  N          <- lavInspect(x, what = "ntotal")
  covv       <- lav_get_vcov(x, parameter_table$parameter_label, standardize)
  covv       <- list(covv)

  list(
    'n' = N,
    'Sigma' = covv,
    'x' = estims,
    'group_parameters' = length(estims),
    'joint_parameters' = 0
  )

}



lav_extract_multi_group <- function(x, standardize) {
  #estims <- lav_get_est(x, standardize)
  parameter_table <- lav_get_est(x, standardize)
  if (standardize) {
    estims <- parameter_table$est.std
  } else {
    estims <- parameter_table$est
  }
  names(estims) <- parameter_table$parameter_label

  #N          <- lavInspect(x, what = "ntotal")
  covv       <- lav_get_vcov(x, parameter_table$parameter_label, standardize)

  #covv <- lav_get_vcov(x, estims, standardize)

  ## Repeat label suffices for number of parameters in each group
  group_labels <- lavInspect(x, what =  "group.label")
  n_groups <- lavInspect(x, what =  "ngroups")
  suffix <- rep(group_labels, each = length(estims) / n_groups)

  ## Boolean indexing for the user-named parameters
  custompara <- parametertable(x)$label
  custompara <- unique(custompara[custompara != ""])
  which_custom <- names(estims) %in% custompara

  ## Removing the .g lavaan uses
  names(estims)  <-
    gsub(
      pattern     = "\\.g[[:digit:]]$",
      x =    names(estims),
      replacement = "",
      perl = TRUE
    )

  if (any(which_custom)){
    names(estims)[!which_custom]     <-
      paste0(names(estims)[!which_custom], ".", suffix[!which_custom])
  } else{
    names(estims) <- paste0(names(estims), ".", suffix)
  }

  rownames(covv) <- colnames(covv)  <- names(estims)

  list('estims' = estims, 'covv' = covv)
}



lav_get_estimates_multi_group_constraints <- function(x, standardize) {
    n_by_group <- lavInspect(x, what = "nobs")
    N <- lavInspect(x, what = "ntotal")

    estims_and_cov <- lav_extract_multi_group(x, standardize)

    if (sum(diff(n_by_group)) != 0) {
      # Warning is given when the groups size is not perfectly equal.
      warning(
        paste0(
          "Since at least some parameter values are constrained to be equal across groups,
          a one group method is used. This method is only valid if group sizes are roughly equal.
          The sample size per group are ",
          paste0(n_by_group , collapse = " and ")
        ),
        call. = FALSE,
        noBreaks. = TRUE
      )
    }

    ## Removing the duplicate parameters that are consistent across groups
    list(
      "x"         = estims_and_cov$estims[unique(names(estims_and_cov$estims))],
      "Sigma"      = list(estims_and_cov$covv[unique(colnames(estims_and_cov$covv)),
                                              unique(rownames(estims_and_cov$covv))]),
      "n"          = N,
      "group_parameters" = length(estims_and_cov$estims),
      "joint_parameters" = 0
    )
  }


lav_get_estimates_multi_group_free <- function(x, standardize) {
  estims_and_cov <- lav_extract_multi_group(x, standardize)
  n_groups <- lavInspect(x, what = "ngroups")
  n_by_group <- lavInspect(x, what = "nobs")

  pars_per_group <- length(estims_and_cov$estims) / n_groups

  covvm <- lapply(1:n_groups, function(Y){
    these_covs <- (1+(Y-1)*pars_per_group):(Y*pars_per_group)
    estims_and_cov$covv[these_covs, these_covs]
  })

list("x"       = estims_and_cov$estims,
    "Sigma"      = covvm,
    "n"         = n_by_group,
    "group_parameters" = length(estims_and_cov$estims) / n_groups,
    "joint_parameters" = 0)

}



