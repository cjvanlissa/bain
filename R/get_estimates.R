#' @importFrom utils tail
rename_estimate <- function(estimate){

  new_names <- names_est <- names(estimate)
  if(any(new_names == "(Intercept)")) new_names[match(new_names, "(Intercept)")] <- "Intercept"
  if(is.null(names_est)){
    if(length(estimate) < 27){
      new_names <- letters[1:length(estimate)]
      warning("The 'estimates' supplied to bain() were unnamed. This is not allowed, because estimates are referred to by name in the 'hypothesis' argument. We renamed the estimates to: ", paste("'", new_names, "'", sep = "", collapse = ", "))
    } else {
      stop("The 'estimates' supplied to bain() were unnamed. This is not allowed, because estimates are referred to by name in the 'hypothesis' argument. Please name your estimates.")
    }
  }

  if(length(new_names) < 3){
    new_names <- gsub("mean of the differences", "difference", new_names)
    new_names <- gsub("mean of ", "", new_names)
  }

  remove_df <- sapply(new_names, grepl, pattern = "[\\]\\$]+", perl = TRUE)
  if(any(remove_df)){
    new_names[remove_df] <- sapply(new_names[remove_df], function(x){
      tmp_split <- strsplit(x, "[\\]\\$]+", perl = TRUE)[[1]]
      if(length(tmp_split)==1){
        x
      } else {
        tail(tmp_split, 1)
        }
    })
  }
  legal_varnames <- sapply(new_names, grepl, pattern = "^[a-zA-Z\\.][a-zA-Z0-9\\._]{0,}$")
  if(!all(legal_varnames)){
    stop("Could not parse the names of the 'estimates' supplied to bain(). Estimate names must start with a letter or period (.), and can be a combination of letters, digits, period and underscore (_).\nThe estimates violating these rules were originally named: ", paste("'", names_est[!legal_varnames], "'", sep = "", collapse = ", "), ".\nAfter parsing by bain, these parameters are named: ", paste("'", new_names[!legal_varnames], "'", sep = "", collapse = ", "), call. = FALSE)
  }
  names(estimate) <- new_names
  estimate
}

#' @title Get estimates from a model object
#' @description Get estimates from a model object, the way the
#' \code{\link{bain}} function will. This convenience function allows you to see
#' that coefficients are properly extracted, note how their names will be parsed
#' by bain, and inspect their values.
#' @param x A model object for which a \code{\link{bain}} method exists.
#' @param ... Parameters passed to and from other functions.
#' @return A named numeric vector.
#' @examples
#' # Example 1
#' m_tt <- t.test(iris$Sepal.Length[1:20], iris$Sepal.Length[21:40])
#' get_estimates(m_tt)
#' # Example 2
#' m_lm <- lm(Sepal.Length ~., iris)
#' get_estimates(m_lm)
#' @rdname get_estimates
#' @export
get_estimates <- function(x, ...){
  UseMethod("get_estimates", x)
}

#' @method get_estimates lm
#' @export
get_estimates.lm <- function(x, ...){
  estimates <- x$coefficients
  variable_types <- sapply(x$model, class)

  if(any(variable_types[-1] == "factor")){
    names(estimates) <- gsub(paste0("^(",
                                   paste(names(x$model)[-1][variable_types[-1] == "factor"], sep = "|"),
                                   ")"), "", names(estimates))
  }
  rename_estimate(estimates)
}

#' @method get_estimates bain_htest
#' @export
get_estimates.bain_htest <- function(x, ...){
  rename_estimate(x$estimate)
}


#' @method get_estimates htest
#' @export
get_estimates.htest <- function(x, ...) {
  stop("To be able to run bain on the results of an object returned by t.test(), you must first load the 'bain' package, and then conduct your t.test. The standard t.test does not return group-specific variances and sample sizes, which are required by bain. When you load the bain package, the standard t.test is replaced by a version that does return this necessary information.")
}


#' @title Label estimates from a model object
#' @description Label estimates from a model object, before passing it on to the
#' \code{\link{bain}} function.
#' @param x A model object for which a \code{\link{bain}} method exists.
#' @param labels Character vector. New labels (in order of appearance) for the
#' model object in \code{x}. If you are unsure what the estimates in \code{x}
#' are, first run \code{\link{get_estimates}}.
#' @param ... Parameters passed to and from other functions.
#' @return A model object of the same class as x.
#' @seealso get_estimates bain
#' @examples
#' # Example 1
#' m_tt <- t.test(iris$Sepal.Length[1:20], iris$Sepal.Length[21:40])
#' m_tt <- label_estimates(m_tt, c("a", "b"))
#' # Example 2
#' m_lm <- lm(Sepal.Length ~., iris)
#' m_lm <- label_estimates(m_lm, labels = c("a", "b", "c", "d", "e", "f"))
#' @rdname label_estimates
#' @export
label_estimates <- function(x, labels, ...){
  UseMethod("label_estimates", x)
}

#' @method label_estimates lm
#' @export
label_estimates.lm <- function(x, labels, ...){
  names_coefs <- names(x$coefficients)
  if(length(names_coefs) != length(labels)) stop("The length of the vector of 'labels' must be equal to the length of the vector of coefficients in the model. To view the vector of coefficients, use 'get_estimates()'.")
  variable_types <- sapply(x$model, class)

  if (any(variable_types[-1] == "factor")) {
    for (fac_name in names(x$model)[-1][variable_types[-1] == "factor"]) {
      fac_levels <- levels(x$model[[fac_name]])
      which_coef <- match(paste0(fac_name, fac_levels), names_coefs)
      fac_levels[which(!is.na(which_coef))] <- labels[which_coef[!is.na(which_coef)]]
      x$model[[fac_name]] <- ordered(x$model[[fac_name]], labels = fac_levels)
    }
  }
  names(x$coefficients) <- labels
  invisible(get_estimates(x))
  x
}

#' @method label_estimates bain_htest
#' @export
label_estimates.bain_htest <- function(x, labels, ...){
  names(x$estimate) <- labels
  invisible(get_estimates(x))
  x
}


#' @method label_estimates htest
#' @export
label_estimates.htest <- function(x, labels, ...) {
  stop("To be able to run bain on the results of an object returned by t.test(), you must first load the 'bain' package, and then conduct your t.test. The standard t.test does not return group-specific variances and sample sizes, which are required by bain. When you load the bain package, the standard t.test is replaced by a version that does return this necessary information.")
}
