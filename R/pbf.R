#' @title Product Bayes Factor
#' @description The product Bayes factor (PBF) aggregates evidence for
#' an informative hypothesis across conceptual replication studies
#' without imposing assumptions about heterogeneity.
#' @param ... Additional arguments passed to `bain`.
#' @return A `data.frame` of class `pbf`.
#' @details Currently, the argument `x` accepts either:
#' * A list of `bain` objects, resulting from a call to `bain`.
#' * A list of model objects for which a `bain` method exists;
#'   in this case, `pbf` will call `bain` on these model objects
#'   before aggregating the Bayes factors.
#' @examples
#' pbf(yi = c(-.33, .32, .39, .31),
#'     vi = c(.085, .034, .016, .071),
#'     ni = c(7, 10, 13, 20))
#' @rdname pbf
#' @references Van Lissa, C. J., Kuiper, R. M., & Clapper, E.
#' (2023, April 25). Aggregating evidence from conceptual
#' replication studies using the product Bayes factor.
#' \doi{10.31234/osf.io/nvqpw}
#' @export
pbf <- function(...){
  UseMethod("pbf")
}

#' @method pbf default
#' @rdname pbf
#' @param x An object for which a method exists, see Details.
#' @export
pbf.default <- function(x, ...){
  if(!all(sapply(x, inherits, what = "bain"))){
    cl <- match.call()
    cl[[1L]] <- quote(bain)
    for(i in (1:length(x))){
      cl[['x']] <- x[[i]]
      x[[i]] <- eval.parent(cl)
    }
    cl[['x']] <- x
    cl[[1L]] <- quote(pbf)
    eval.parent(cl)
  }

  # Merge the hypotheses from list item 1 and 2 into object merged
  if(length(x) > 1){
    hyps <- x[[1]]$hypotheses
    for(i in length(x)-1){
      hyps <- c(hyps, x[[i+1]]$hypotheses)
      # Drop all non-duplicated hypotheses from merged
      hyps <- hyps[duplicated(hyps)]
      # If merged now has length 0, throw error
      if(length(hyps) == 0){
        stop("The objects passed to pbf() have no hypotheses in common.")
      }
      # Else, go back to step 1, but now merge merged with list item 3
    }
  }
  BFs <- do.call(cbind, lapply(x, function(y){y$fit$BF.c[match(hyps, y$hypotheses)]}))
  colnames(BFs) <- paste0("Sample ", 1:ncol(BFs))
  res <- data.frame(PBF = apply(BFs, 1, prod), BFs)# obtain pbf ic, might need to change dependent on alternative hyp
  rownames(res) <- paste0(sprintf('H%d: ', 1:length(hyps)),hyps) # give names
  class(res) <- c("pbf", class(res))
  return(res)
}

#' @method pbf numeric
#' @export
#' @rdname pbf
#' @param yi Numeric vector with the observed effect sizes.
#' @param vi Numeric vector with the observed sampling variances.
#' @param ni Integer vector with the sample sizes.
#' @param hypothesis A character string containing the informative hypotheses to evaluate.
pbf.numeric <- function(yi, vi, ni, hypothesis = "y = 0", ...){
  est <- c("y" = 0)
  hypars <- params_in_hyp(hypothesis)
  if(!length(hypars) == 1) stop("The hypothesis may reference only a single parameter when using pbf() with arguments 'yi' and 'vi'.")
  names(est) <- hypars[1]
  bain_list <- mapply(FUN = function(y, v, n){
    est[1] <- y
    bain(x = est,
         Sigma = matrix(v, 1, 1),
         n = n,
         hypothesis = hypothesis,
         joint_parameters = 1,
         ...)},
    y = yi, v = vi, n = ni, SIMPLIFY = FALSE)
  cl <- call("bain:::pbf.default")
  cl[["x"]] <- bain_list
  cl[["hypothesis"]] <- hypothesis
  eval.parent(cl)
}
