#' library(motley)
#' library(MplusAutomation)
#' mg_ex <- readModels("binary/ex5.14.out")
#' gorica(readModels("c:/git_repositories/tmp2_s.out"), "Intercepts.M_3.H>Intercepts.DE_4.H")
#' tmp3 <- readModels("C:/Git_Repositories/conflict_spillover/mplus/16-04-2018_ang_m_dsem.out")
#' par_spec <- tmp3$tech1$parameterSpecification
#' tmp <- get_estimates(readModels("binary/ex9.11.out"))
#' tmp2 <- get_estimates(readModels("../gorica/binary/ex3.12.out"))
#' tmp5 <- readModels("binary/ex9.2c.out")
#' tmp5$
#' x <- tmp5
#' par_spec <- tmp4$tech1$parameterSpecification
#' estimate <- tmp4$parameters$unstandardized
#' @method get_estimates mplus.model
#' @export
mplus_estimates_only <- function(x, ...){
  techs_missing <- c("tech1" = is.null(x[["tech1"]]), "tech3" = is.null(x[["tech3"]]))
  techs_missing <- sapply(names(techs_missing)[!techs_missing], function(i){!length(x[[i]]) > 0})
  if(any(techs_missing)) stop("Need the ", paste(names(techs_missing)[techs_missing], collapse = " and "), " output in order to derive the covariance matrix of the estimates from Mplus. Please re-run your analysis in Mplus, adding the following syntax:\nOUTPUT: tech1 tech3;")

  par_spec <- x$tech1$parameterSpecification
  estimate <- x$parameters$unstandardized[!x$parameters$unstandardized$pval == 999, ]
  if("group.names" %in% names(attributes(par_spec))){
    if ("THE.ADDITIONAL.PARAMETERS" %in% names(par_spec)){
      par_spec[["THE.ADDITIONAL.PARAMETERS"]] <- NULL
    }

    param_id <- do.call(rbind, lapply(names(par_spec), function(x) {
      cbind(get_param_id(par_spec[[x]]), group_label = x, stringsAsFactors = FALSE)
    }))

    est_label <- estimate[, -c(1:6), drop = FALSE]
    groups <- toupper(apply(est_label[, ncol(est_label):1, drop = FALSE], 1, paste, collapse = "."))
    if(!all(unique(groups) == unique(param_id[, 3]))){
      groups <- toupper(apply(est_label, 1, paste, collapse = "."))
      if(!all(unique(groups) == unique(param_id[, 4]))) stop("Could not match parameter estimates to tech1 output: Group labels did not match between parameters and tech1.")
    }
    param_id$label <- paste(param_id$label, param_id$group_label, sep = ".")
    estimate$label <- paste(estimate$paramHeader, estimate$param, groups, sep = ".")


  } else {
    param_id <- get_param_id(par_spec)
    estimate$label <- paste(estimate$paramHeader, estimate$param, sep = ".")
  }

  match_lab <- gsub("^Residual\\.", "", estimate$label)
  match_lab <- gsub("^Means", "Intercepts", match_lab)
  match_lab <- match(param_id$label, match_lab)
  if(any(duplicated(match_lab))) stop("Could not match parameter estimates to tech1 output: Several duplicate matches.")
  if(anyNA(match_lab)) stop("Could not match parameter estimates to tech1 output: Several estimates were not matched by parameters in tech1.")


  data.frame(estimate[match_lab, ], id = param_id$id)
}

get_estimates.mplus.model <- function(x, ...){
  Arg <- match.call()
  if(!"estimate" %in% names(Arg)){
    estimate <- mplus_estimates_only(x)
  }
  duplicates <- NULL
  if(any(duplicated(estimate$id))){
    dups <- estimate[duplicated(estimate$id), ]
    estimate <- estimate[!duplicated(estimate$id), ]
    duplicates <- lapply(unique(dups$id), function(o){dups$label[dups$id == o]})
    names(duplicates) <- estimate$label[match(dups$id, estimate$id)]
  }
  estimate <- estimate[order(estimate$id), ]



  coefs <- estimate$est
  names(coefs) <- estimate$label

  Sigma <- x$tech3$paramCov[estimate$id, estimate$id]
  Sigma[upper.tri(Sigma)] <- t(Sigma)[upper.tri(Sigma)]

  colnames(Sigma) <- estimate$label
  rownames(Sigma) <- estimate$label

  out <- list(estimate = coefs,
              Sigma = Sigma)
  if(!is.null(duplicates)) out[["label_synonyms"]] <- duplicates
  class(out) <- "bain_estimate"
  out
}

# tau vector thresholds
# nu vector means OR intercepts of continuous observed variables
# lambda matrix factor loadings; rows == dependent; columns == latent
# theta matrix residual variances and covariances of the observed dependent
#              variables or the latent response variables
# alpha vector means OR intercepts of continuous latent variables
# beta matrix regression coefficients for continuous latent variables on
#             continuous latent variables
# gamma matrix regression coefficients for continuous latent variables on
#              observed independent variables. Rows represent latent variables.
#              columns represent the observed independent variables.
# psi matrix variances and covariances of continuous latent variables
# delta vector scaling information for observed dependent variables


.mplus_tech1_sections <- c("tau" = "Thresholds", "nu" = "Intercepts", "lambda" = ".BY", "theta" = ".WITH", "alpha" = "Intercepts", "beta" = ".ON", "gamma" = ".ON", "psi" = ".WITH", "delta" = "bla")
# c(
#   "tau", "vector", "Thresholds", "",
#   "nu", "vector", "Intercepts", "",
#   "lambda", "matrix", ".BY", "",
#   "alpha", "vector", "Intercepts", "",
#   "beta", "matrix", ".ON", "",
#   "gamma", "matrix", ".ON", "",
#   "psi", "matrix", ".WITH", "",
#   "delta", "vector", "bla", ""
# )
# When saving analysis results, the parameters are saved in the order used in the parameter specification matrices.

get_param_id <- function(par_spec) {
  these_secs <-
    names(par_spec)[na.omit(match(names(.mplus_tech1_sections), names(par_spec)))]
  if (length(these_secs) == 0)
    return(NULL)
  out <- do.call(rbind, lapply(these_secs, function(x) {
    sec <- par_spec[[x]]
    if (x %in% c("tau", "nu", "alpha", "delta")) {
      cbind(.mplus_tech1_sections[x],
            colnames(sec),
            as.vector(sec))
    } else {
      if (x %in% c("psi", "theta")) {
        tmp <- cbind("Variances", names(diag(sec)), diag(sec))
        diag(sec) <- NA
        rbind(tmp, cbind(rep(paste0(rownames(sec), ".WITH"), each = ncol(sec)),
              rep(colnames(sec), nrow(sec)),
              as.vector(t(sec))))
      } else {
        if (x == "lambda") {
          cbind(rep(paste0(colnames(sec), ".BY"), each = nrow(sec)),
                rep(rownames(sec), ncol(sec)),
                as.vector(sec))
        } else {
          if (x %in% c("beta", "gamma")) {
            cbind(rep(paste0(rownames(sec), ".ON"), each = ncol(sec)),
                  rep(colnames(sec), nrow(sec)),
                  as.vector(t(sec)))
          }
        }
      }
    }

  }))
  out <-
    data.frame(out[!(is.na(out[, 3]) |
                       out[, 3] == "0"), ], stringsAsFactors = FALSE)

  out$label <- apply(out[, -3], 1, paste, collapse = ".")
  out$id <- as.integer(out$X3)
  out[order(out$id), c("label", "id")]
}

#' @method print gorica_estimate
#' @export
print.gorica_estimate <- function(x,
                         digits = 3,
                         na.print = "", ...){
  dat <- x$estimate
  dat <- formatC(dat, digits = digits, format = "f")
  print(dat, quote = FALSE)
}


