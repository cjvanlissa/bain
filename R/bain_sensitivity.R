#' @title Sensitivity analysis for bain
#' @description Conducts a sensitivity analysis for \code{\link[bain]{bain}},
#' see details.
#' \code{fractions} argument, and returns a list of bain results objects.
#' @param x PARAM_DESCRIPTION
#' @param hypothesis PARAM_DESCRIPTION
#' @param fractions PARAM_DESCRIPTION, Default: 1
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details The Bayes factor for equality constraints is sensitive to a
#' scaling factor applied to the prior distribution. The \code{fraction}
#' argument adjusts this scaling factor. The function \code{bain_sensitivity}
#' is a wrapper for \code{\link[bain]{bain}}, which accepts a vector for the
#' \code{fractions} argument, and returns a list of bain results objects.
#' A table with a sensitivity analysis for specific statistics can be obtained
#' using the \code{summary()} function, which accepts the argument
#' \code{summary(which_stat = ...)}. The available statistics are elements of
#' the \code{$fit} table (Fit_eq, Com_eq, Fit_in, Com_in, Fit, Com, BF, PMPa,
#' and PMPb), and elements of the
#' \code{BFmatrix}, which can be accessed by matrix notation, e.g.:
#' \code{summary(bain_sens, which_stat = "BFmatrix[1,2]")}.
#' @examples
#' res <- t_test(extra ~ group, data = sleep)
#' bain(res, hypothesis = "group1 - group2 = 1.5")
#' bain_sens <- bain_sensitivity(res, hypothesis = "group1-group2 = 1.5",
#'                               fractions = c(1,2,3))
#' summary(bain_sens, which_stat = "BF")
#' summary(bain_sens, which_stat = "BFmatrix[1,1]")
#' @rdname bain_sensitivity
#' @export
bain_sensitivity <- function(x, hypothesis, fractions = 1, ...){
  Args <- as.list(match.call()[-1])
  outlist <- lapply(fractions, function(this_frac){
    Args[["fraction"]] <- this_frac
    out <- do.call(bain, Args)
    out[["fraction"]] <- this_frac
    out
  })
  class(outlist) <- c("bain_sensisitity", class(outlist))
  outlist
}

summary.bain_sensitivity  <- function(object, which_stat = "BF", ...){
  which_stat <- as.character(which_stat)[1]
  if(!grepl("BFmatrix\\[", which_stat)){
    outlist <- lapply(object, function(this_output){
      this_output[["fit"]][which_stat]
    })
    outlist <- tryCatch({
      lapply(object, function(this_output){
        this_output[["fit"]][which_stat]
      })
    }, error = function(e){stop("Requested statistic is not part of the 'fit' table. The fit table has the following columns: ", paste0(names(object[[1]]$fit), collapse = ", "), ".", call. = FALSE)})
  } else {
    outlist <- tryCatch({
      lapply(object, function(this_output){
        if(!grepl("drop", which_stat)){
          which_stat <- gsub("\\]", ", drop = FALSE\\]", which_stat)
        }
        out <- eval(parse(text = which_stat), envir = this_output)
        if(!is.null(dim(out))){
          tmp <- as.data.frame.table(out)
          out <- tmp$Freq
          names(out) <- paste0(tmp$Var1, ".", tmp$Var2)
        }
        out
      })
    }, error = function(e){stop("Requested cells that are not part of BFmatrix. BFmatrix has ", dim(object[[1]]$BFmatrix)[1], " rows and ", dim(object[[1]]$BFmatrix)[2], " columns.", call. = FALSE)})
  }
  out_tab <- data.frame(Fraction = sapply(object, `[[`, "fraction"),
                        t(do.call(cbind,
                                  outlist)))
  rownames(out_tab) <- NULL
  class(out_tab) <- c("sum_sensitivity", class(out_tab))
  out_tab
}
