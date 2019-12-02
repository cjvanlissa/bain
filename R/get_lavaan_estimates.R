#' @importFrom utils getFromNamespace
#' @importFrom lavaan parametertable lavInspect standardizedsolution
lav_getParameterLabels <-
  getFromNamespace("getParameterLabels", "lavaan")

lav_get_estimates <- function(x, standardize, retain_which = c("=~", "~", "~1"), split_multigroup_sigma = TRUE, allow_between_constraints = FALSE) {

  # Prepare output skeleton -------------------------------------------------

  out_list <- list(estimate = NULL,
                   Sigma = NULL,
                   n = lavInspect(x, what = "ntotal"),
                   group_parameters = NULL,
                   joint_parameters = NULL
  )

  num_groups <- lavInspect(x, what = "ngroups")

  # Get estimates -----------------------------------------------------------

  unst_pars <- parametertable(x)
  between_group_constraints <- FALSE

  if(any(unst_pars$op == "==")){ # Maybe use op == "=="
    constraints <- unst_pars[unst_pars$op == "==", ]
    unst_pars <- unst_pars[!unst_pars$op == "==", ]
  } else {
    constraints <- NULL
  }

  parameter_table <- cbind(unst_pars, standardizedsolution(x))

  ## Only the free parameters or parameters explicitly defined by user.
  parameter_table <- parameter_table[parameter_table$free > 0 & !parameter_table$plabel == "", ]

  parameter_table$bain_label <- parameter_table$parameter_label <- lav_getParameterLabels(partable = parameter_table)
  if(num_groups > 1){
    if(!is.null(constraints)){
      constraints
      between_group_constraints <- any(!all(parameter_table$group[match(constraints$lhs, parameter_table$plabel)] == parameter_table$group[match(constraints$rhs, parameter_table$plabel)]))
    }
    if(between_group_constraints & !allow_between_constraints){
      stop("Cannot evaluate hypotheses for multiple group lavaan models with between-group constraints.", call. = FALSE)
    }

    # Label group params ------------------------------------------------------

    group_labels <- lavInspect(x, what = "group.label")

    ## Boolean indexing for the user-named parameters
    custompara <- parameter_table$label
    custompara <- unique(custompara[custompara != ""])
    which_custom <- parameter_table$parameter_label %in% custompara

    ## Removing the .g lavaan uses
    parameter_table$bain_label <- gsub(pattern = "\\.g[[:digit:]]$",
                                       x = parameter_table$bain_label,
                                       replacement = "", perl = TRUE)

    if (any(which_custom)){
      parameter_table$bain_label[!which_custom] <-
        paste0(parameter_table$bain_label[!which_custom], ".", group_labels[parameter_table$group][!which_custom])
    } else {
      parameter_table$bain_label <- paste0(parameter_table$bain_label, ".", group_labels[parameter_table$group])
    }

  }

  # Retain only requested parameters
  parameter_table <- parameter_table[parameter_table$op %in% retain_which, ]

  # Remove duplicates
  parameter_table <- parameter_table[!duplicated(parameter_table$parameter_label), ]

  if (standardize) {
    out_list$estimate <- parameter_table$est.std
  } else {
    out_list$estimate <- parameter_table$est
  }

  #names(out_list$estimate) <- rename_function(parameter_table$bain_label)
  names(out_list$estimate) <- parameter_table$bain_label

  # Get covariance ----------------------------------------------------------

  if (standardize) {
    out_list$Sigma <- list(lavInspect(x, "vcov.std.all")[parameter_table$parameter_label, parameter_table$parameter_label, drop = FALSE])
  } else {
    out_list$Sigma <- list(lavInspect(x, "vcov")[parameter_table$parameter_label, parameter_table$parameter_label, drop = FALSE])
  }

  # Rename Sigma ------------------------------------------------------------

  colnames(out_list$Sigma[[1]]) <- rownames(out_list$Sigma[[1]]) <- names(out_list$estimate)

  # Probably move this to multigroup section --------------------------------

  out_list$group_parameters = length(out_list$estimate)
  out_list$joint_parameters = 0


  # Handle multi-group ------------------------------------------------------
  if(num_groups > 1){
    # Things that need to be done for multigroup anyway
    out_list$n <- lavInspect(x, what = "nobs")
    # Things specific to bain
    if(split_multigroup_sigma){
      if(!between_group_constraints){

        pars_per_group <- length(out_list$estimate) / num_groups
        out_list$Sigma <- lapply(1:num_groups, function(Y){
          these_covs <- (1+(Y-1)*pars_per_group):(Y*pars_per_group)
          out_list$Sigma[[1]][these_covs, these_covs]
        })

        out_list$group_parameters <- pars_per_group

      } else {
        # Warning is given when the groups size is not perfectly equal.
        if (sum(diff(out_list$n)) != 0) {
          warning(
            paste0(
              "Since at least some parameter values are constrained to be equal across groups,
          a one group method is used. This method is only valid if group sizes are roughly equal.
          The sample size per group are ",
              paste0(out_list$n , collapse = " and ")
            ),
            call. = FALSE,
            noBreaks. = TRUE
          )
          out_list$n <- lavInspect(x, what = "ntotal")
        }
      }
    }
  }
  out_list
}
