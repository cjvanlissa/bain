#' @importFrom utils getFromNamespace
lav_getParameterLabels <-
  getFromNamespace("getParameterLabels", "lavaan")

#' @importFrom lavaan lavInspect parametertable standardizedsolution
lav_get_est <- function(x, standardize) {
  unst_pars <- parametertable(x)
  if(length(unique(unst_pars$group)) > 1){
    if(any(unst_pars$op == "==")){
      if(any(unst_pars$op == "==" & unst_pars$group == 0)){
        stop("Bain cannot evaluate hypotheses for multiple group lavaan models with between-group constraints.", call. = FALSE)
      }
    }
  }
  estims <- cbind(unst_pars,
                  standardizedsolution(x))
  #standardizedsolution(x)[, c("est.std", "se")]
  ## Only the free parameters or parameters explicitly defined by user.
  estims <- estims[estims$free > 0 & nchar(estims$plabel) > 0, ]
  variance_parameters <- estims$lhs == estims$rhs & estims$op == "~~"
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
  list(covv[param_labels, param_labels, drop = FALSE])
}



lav_get_estimates <- function(x, standardize) {
  parameter_table <- lav_get_est(x, standardize)
  # Drop coefficients we cannot handle
  #parameter_table <- parameter_table[!parameter_table$op %in% c("~~", ":="), ]
  parameter_table <- parameter_table[parameter_table$op %in% c("=~", "~", "~1"), ]
  if (standardize) {
    estims <- parameter_table$est.std
  } else {
    estims <- parameter_table$est
  }
  names(estims) <- parameter_table$parameter_label
  covv       <- lav_get_vcov(x, parameter_table$parameter_label, standardize)
  out_list <- list(x = estims,
                   n = lavInspect(x, what = "ntotal"),
                   Sigma = covv,
                   group_parameters = length(estims),
                   joint_parameters = 0
                   )

# Handle multi-group ------------------------------------------------------
  num_groups             <- lavInspect(x, what = "ngroups")

  if(num_groups > 1) {
    out_list <- lav_label_multi_group(x, out_list, parameter_table)
    if(!any(parameter_table$op == "==")){
      out_list <- lav_bain_multi_group_free(x, out_list)
    } else {
      out_list <- lav_bain_multi_group_constraints(x, out_list)
    }
  }# else {
  #  colnames(out_list$Sigma[[1]]) <- rownames(out_list$Sigma) <- rename_function(colnames(out_list$Sigma))
  #}
  names(out_list$x) <- rename_function(names(out_list$x))

  out_list$Sigma <- lapply(out_list$Sigma, function(x){
    colnames(x) <- rownames(x) <- rename_function(colnames(x))
    x
  })
  out_list
}


lav_label_multi_group <- function(x, out_list, parameter_table) {
  ## Repeat label suffices for number of parameters in each group
  group_labels <- lavInspect(x, what = "group.label")
  #n_groups <- lavInspect(x, what = "ngroups")
  #suffix <- rep(group_labels, each = length(out_list$x) / n_groups)

  ## Boolean indexing for the user-named parameters
  custompara <- parameter_table$label
  custompara <- unique(custompara[custompara != ""])
  which_custom <- names(out_list$x) %in% custompara

  ## Removing the .g lavaan uses
  names(out_list$x)  <-
    gsub(pattern     = "\\.g[[:digit:]]$", x =    names(out_list$x),
         replacement = "", perl = TRUE)

  if (any(which_custom)){
    names(out_list$x)[!which_custom]     <-
      paste0(names(out_list$x)[!which_custom], ".", group_labels[parameter_table$group][!which_custom])
  } else{
    names(out_list$x) <- paste0(names(out_list$x), ".", group_labels[parameter_table$group])
  }
  out_list$Sigma <- lapply(out_list$Sigma, function(x){
    rownames(x) <- colnames(x)  <- names(out_list$x)
    x
  })

  out_list
}



lav_bain_multi_group_constraints <- function(x, out_list) {
    n_by_group <- lavInspect(x, what = "nobs")#[match(lavInspect(fit1, what = "group.label"))]
    N_total <- lavInspect(x, what = "ntotal")
    # Warning is given when the groups size is not perfectly equal.
    if (sum(diff(n_by_group)) != 0) {
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
    out_list$x <- out_list$x[unique(names(out_list$x))]
    out_list$Sigma <- out_list$Sigma[unique(colnames(out_list$Sigma)),
                                     unique(rownames(out_list$Sigma))]
    out_list$group_parameters <- length(out_list$x)
    out_list
    }

lav_bain_multi_group_free <- function(x, out_list) {
  n_groups <- lavInspect(x, what = "ngroups")
  n_by_group <- lavInspect(x, what = "nobs")
  pars_per_group <- length(out_list$x) / n_groups
  out_list$Sigma <- lapply(1:n_groups, function(Y){
    these_covs <- (1+(Y-1)*pars_per_group):(Y*pars_per_group)
    out_list$Sigma[[1]][these_covs, these_covs]
  })
  out_list$n <- n_by_group
  out_list$group_parameters <- pars_per_group
  out_list
}

#' @method coef lavaan
#' @export
coef.lavaan <- function(object, standardize = FALSE, ...){
  pars <- lav_get_est(object, standardize)
  out <- pars[[c("est", "est.std")[standardize+1]]]
  names(out) <- pars$parameter_label
  out
}

#' @method vcov lavaan
#' @export
vcov.lavaan <- function(object, standardize = FALSE, ...){
  pars <- lav_get_est(object, standardize)
  lav_get_vcov(object, pars$parameter_label, standardize)[[1]]
}

#' @method nobs lavaan
#' @export
nobs.lavaan <- function(object, ...){
  lavInspect(object, what = "ntotal")
}

