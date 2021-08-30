#' Bayes factors for informative hypotheses
#'
#' \code{bain} is an acronym for "Bayesian informative hypotheses evaluation".
#' It uses the Bayes factor to evaluate hypotheses specified using equality and
#' inequality constraints among (linear combinations of) parameters in a wide
#' range of statistical models. A tutorial by Hoijtink, Mulder, van Lissa,
#' and Gu (2018), was published in
#' \href{https://www.doi.org/10.1037\%2Fmet0000201}{Psychological Methods}.
#' The preprint of that tutorial is available at
#' \href{https://psyarxiv.com/v3shc/}{DOI:10.31234/osf.io/v3shc}, or on the bain
#' website at
#' \url{https://informative-hypotheses.sites.uu.nl/software/bain/}
#' \strong{Users are
#' advised to read the tutorial AND the vignette that is provided
#' with this package before using} \code{bain}.
#'
#' @param x An R object containing the outcome of a statistical analysis.
#' Currently, the following objects can be processed: \code{lm()},
#' \code{t_test()}, \code{lavaan} objects created with the
#' \code{sem()}, \code{cfa()}, and \code{growth()} functions, and named
#' vector objects. See the vignette for elaborations.
#' @param hypothesis	A character string containing the informative hypotheses
#' to evaluate. See the vignette for elaborations.
#' @param fraction A number representing the fraction of information
#' in the data used to construct the prior distribution
#' (see the tutorial DOI: 10.1037/met0000201): The default value 1 denotes the
#' minimal fraction, 2 denotes twice the minimal fraction, etc.
#' @param ... Additional arguments. See the vignette for elaborations.
#'
#' @return The main output resulting from analyses with \code{bain} are
#' Bayes factors and posterior model probabilities associated with the
#' hypotheses that are evaluated. See the \strong{tutorial} and the
#' \strong{vignette} for further elaborations.
#'
#' @author The main authors of the bain package are Xin Gu, Caspar
#' van Lissa, Herbert Hoijtink and Joris Mulder. Contributions
#' were made by Marlyne Bosman and Camiel van Zundert.
#' Contact information can be
#' found on the bain website at
#' \url{https://informative-hypotheses.sites.uu.nl/software/bain/}
#'
#' @references See the vignette for additional references.
#'
#' Hoijtink, H., Mulder, J., van Lissa, C., and Gu, X. (2018). A tutorial on
#' testing hypotheses using the Bayes factor. \emph{Psychological Methods.}
#' DOI: 10.1037/met0000201
#'
#' @examples
#' # USING BAIN WITH A LM OBJECT: Bayesian ANOVA
#' # make a factor of variable site
#' sesamesim$site <- as.factor(sesamesim$site)
#' # execute an analysis of variance using lm() which, due to the -1, returns
#' # estimates of the means per group
#' anov <- lm(postnumb~site-1,sesamesim)
#' # take a look at the estimated means and their names
#' coef(anov)
#' # set a seed value
#' set.seed(100)
#' # use the names to formulate and test hypotheses with bain
#' results <- bain(anov, "site1=site2=site3=site4=site5; site2>site5>site1>
#' site3>site4")
#' #
#' # USING BAIN WITH A NAMED VECTOR: Bayesian ANOVA
#'
#' # make a factor of variable site
#' sesamesim$site <- as.factor(sesamesim$site)
#' # execute an analysis of variance using lm() which, due to the -1, returns
#' # estimates of the means per group
#' anov <- lm(postnumb~site-1,sesamesim)
#' # collect the estimates means in a vector
#' estimate <- coef(anov)
#' # give names to the estimates in anov
#' names(estimate) <- c("site1", "site2", "site3","site4","site5")
#' # create a vector containing the sample sizes of each group
#' ngroup <- table(sesamesim$site)
#' # compute the variance of the means and collect them in a list
#' var <- summary(anov)$sigma**2
#' cov1 <- matrix(var/ngroup[1], nrow=1, ncol=1)
#' cov2 <- matrix(var/ngroup[2], nrow=1, ncol=1)
#' cov3 <- matrix(var/ngroup[3], nrow=1, ncol=1)
#' cov4 <- matrix(var/ngroup[4], nrow=1, ncol=1)
#' cov5 <- matrix(var/ngroup[5], nrow=1, ncol=1)
#' covlist <- list(cov1, cov2, cov3, cov4,cov5)
#' # set a seed value
#' set.seed(100)
#' # test hypotheses with bain. Note that there are multiple groups
#' # characterized by one mean, therefore group_parameters=1. Note that
#' # there are no joint parameters, therefore, joint_parameters=0.
#' results <- bain(estimate,
#' "site1=site2=site3=site4=site5; site2>site5>site1>site3>site4",
#' n=ngroup,Sigma=covlist,group_parameters=1,joint_parameters = 0)
#'
#' # SEE THE TUTORIAL AND VIGNETTE FOR MANY ADDITIONAL EXAMPLES
#'
#'
#' @rdname bain
#' @export
#' @useDynLib bain, .registration = TRUE
#' @importFrom stats as.formula coef complete.cases cov lm model.frame
#' model.matrix pt qt sd setNames summary.lm var vcov
#'
bain <- function(x, hypothesis, fraction = 1, ...) {
  UseMethod("bain", x)
}


#' @method bain lm
#' @export
bain.lm <-
  function(x,
           hypothesis,
           fraction = 1,
           ...,
           standardize = FALSE) {
    cl <- match.call()
    Args <- as.list(cl[-1])
    if(!("numeric" %in% class(x$coefficients)) | !is.null(dim(x$coefficients))){
      stop("It appears that you are trying to run a multivariate linear model. This cannot be done using a lm() object as input for bain. Instead use a named numeric vector. See vignette('Introduction_to_bain') for further information")
    }
    # Checken of het factor OF ordered factor is!!!!!!
    # Nu wordt overal (?) factor_variables[-1] gebruikt. Kan het niet gewoon één keer hier [-1]?
    factor_variables <- sapply(x$model[-1], inherits, what = "factor")
    Warnings <- NULL
    # Checken of het factor OF ordered factor is!!!!!!
    which_model <- if(any(factor_variables)){
        if(ncol(x$model) == 2){
          "ANOVA"
        } else {
          if(sum(factor_variables) == 1 & # If there's only one factor
             !grepl(":", paste(names(x$coefficients), collapse = ""))){ # AND no interactions between that factor and continuous predictors
            "ANCOVA"
          } else {
            Warnings <- c(Warnings,
                          "Calling bain on an object of type 'lm' with mixed predictors (factors and numeric predictors) may result in untrustworthy results. Please interpret with caution."
            )
            "mixed predictors"
          }
        }
      } else {
        "continuous predictors"
      }

    switch(which_model,
           ANOVA = {
             if(names(x$coefficients)[1] == "(Intercept)"){
               Warnings <- c(Warnings, "Your ANOVA model included an intercept, which was dropped by bain. Please specify your hypothesis in terms of group means.")
             }
             n <- table(x$model[, 2])
             anovafm <- lm(x$model[, 1] ~ -1 + x$model[, 2])
             Args$x <- anovafm$coefficients
             names(Args$x) <- paste0(names(x$model)[2], levels(x$model[, 2]))
             Args$Sigma <- lapply((1/n * summary.lm(anovafm)$sigma**2), matrix)
             Args$group_parameters <- 1
             Args$joint_parameters <- 0
             Args$n <- n
           },
           ANCOVA = {
             if(names(x$coefficients)[1] == "(Intercept)"){
               Warnings <- c(Warnings, "Your ANCOVA model included an intercept, which was dropped by bain. Please specify your hypothesis in terms of group means.")
             }
             df <- x$model
             var_factor <- which(factor_variables)+1
             var_numeric <- c(FALSE, sapply(x$model[-1], is.numeric))
             # Altijd scalen? Waarom? Dit gaat er vanuit dat de gebruiker nooit de gecontrolleerde gemiddelden wil weten
             df[, var_numeric] <- scale(df[, var_numeric], scale = FALSE)

             ##analysis
             ancovafm <-  lm(as.formula(paste0(names(df)[1], "~ -1 + ",
                                               names(x$model)[var_factor], " + ",
                                               paste(names(x$model)[var_numeric], collapse = " + ")
                                               )), df)

             Args$x <- ancovafm$coefficients
             resvar <- summary(ancovafm)$sigma**2
             Args$Sigma <- by(cbind(1, df[, var_numeric]), df[[var_factor]], function(x){
               resvar * solve(t(as.matrix(x)) %*% as.matrix(x))
             })
             Args$group_parameters <- 1
             Args$joint_parameters <- sum(var_numeric)
             Args$n <- table(df[[var_factor]])
           },
           {

             dependent <- x$model[, 1]
             predictor <- model.matrix(as.formula(x$call[2]), x$model)

             hyp_params <- params_in_hyp(hypothesis)

             # Partial matching implemented here
             coef_in_hyp <- charmatch(rename_function(hyp_params),
                                   rename_function(names(x$coefficients)))

             if(anyNA(coef_in_hyp)){
               stop("Some of the parameters referred to in the 'hypothesis' do not correspond to parameter names of object 'x'.\n  The following parameter names in the 'hypothesis' did not match any parameters in 'x': ",
                    paste(reverse_rename_function(hyp_params[is.na(coef_in_hyp)]), collapse = ", "),
                    "\n  The parameters in object 'x' are named: ",
                    paste(reverse_rename_function(names(x$coefficients)), collapse = ", "))
             }
             if(any(coef_in_hyp == 0)){
               stop("Some of the parameters referred to in the 'hypothesis' matched multiple parameter names of object 'x'.\n  The following parameter names in the 'hypothesis' matched multiple parameters in 'x': ",
                    paste(reverse_rename_function(hyp_params[coef_in_hyp == 0]), collapse = ", "),
                    "\n  The parameters in object 'x' are named: ",
                    paste(reverse_rename_function(names(x$coefficients)), collapse = ", "))
             }

             if (!standardize) {
               estimate <- coef(x)[coef_in_hyp]
               Sigma <- vcov(x)[coef_in_hyp, coef_in_hyp]
             } else{
               ses <- seBeta(
                 predictor[,-1],
                 dependent,
                 Nobs = sum(complete.cases(x$model)),
                 alpha = .05,
                 estimator = 'Normal'
               )
               select_parameters <- match(names(x$coefficients)[coef_in_hyp], colnames(predictor)[-1], )
               #select_parameters <- select_parameters[na.omit(select_parameters)]
               estimate <- ses$CIs$estimate[select_parameters]
               names(estimate) <- colnames(predictor)[-1][select_parameters]
               Sigma <- ses$cov.mat[select_parameters, select_parameters, drop = FALSE]
               rownames(Sigma) <- colnames(Sigma) <- names(estimate)
             }
             Args$x <- estimate
             Args$Sigma <- Sigma
             Args$group_parameters <- 0
             Args$joint_parameters <- length(coef_in_hyp)
             Args$n <- nrow(x$model)
           }
           )

    Bain_res <- do.call(bain, Args)
    Bain_res$call <- cl
    Bain_res$model <- x

    if(!is.null(Warnings)){
      Bain_res$Warnings <- Warnings
    }
    class(Bain_res) <- c("bain_lm", class(Bain_res))
    attr(Bain_res, "which_model") <- which_model
    Bain_res
  }



#' @method bain lavaan
#' @importFrom lavaan parTable lavInspect
#' @export
bain.lavaan <- function(x, hypothesis, fraction = 1, ..., standardize = FALSE) {
  cl <- match.call()
  Args <- as.list(cl[-1])
  if(standardize){
    if(any(parTable(x)$op == "==")) stop("Cannot yet evaluate hypothesis on standardized model coefficients if there are equality constraints in the model.")
  }
  num_levels         <- lavInspect(x, what = "nlevels")
  if(grepl("~~", hypothesis)){
    stop("Bain cannot yet handle hypotheses about (co)variance parameters in lavaan models.")
  }
  if (num_levels > 1) {
    # Multilevel structure in data: Gives warning since this aspect is not integrated.
    stop(
      paste(
        "Lavaan object contains",
        num_levels ,
        "levels. Multilevel structures are not yet integrated in Lav_in_Bain."
      )
    )
  }

  Args[c("x", "Sigma", "n", "group_parameters", "joint_parameters")] <- lav_get_estimates(x, standardize)
  Args$hypothesis <-  hypothesis
  #browser()
  Bain_res <- do.call(bain, Args)
  Bain_res$call <- cl
  Bain_res$model <- x
  class(Bain_res) <- c("bain_lavaan", class(Bain_res))

  Bain_res$hypotheses <- reverse_rename_function(Bain_res$hypotheses)
  names(Bain_res$estimates) <- reverse_rename_function(names(Bain_res$estimates))
  Bain_res
}



#' @method bain htest
#' @export
bain.htest <-
  function(x,
           hypothesis,
           fraction = 1,
           ...) {
    stop("The standard t.test() function from the 'stats' package does not return variance and sample size, which are required to run bain. Please use the function t_test() from the 'bain' package instead. It accepts the same arguments.")
}

#' @method bain t_test
#' @export
bain.t_test <-
  function(x,
           hypothesis,
           fraction = 1,
           ...) {
      cl <- match.call()
      Args <- as.list(cl[-1])
      ests <- get_estimates(x)
      Args$x <- ests$estimate
      Args$n <- x$n

      if(length(x$estimate) == 1){
        Args$Sigma <- x$v/x$n
        Args$group_parameters <- 0
        Args$joint_parameters <- 1
      } else {
        if (!x$method %in% c(" Two Sample t_test", " Two Sample t-test")) {
          Args$Sigma <- lapply(x$v/x$n, as.matrix)
        } else {
          df <- sum(x$n) - 2
          v <- 0
          if (x$n[1] > 1)
            v <- v + (x$n[1] - 1) * x$v[1]
          if (x$n[2] > 1)
            v <- v + (x$n[2] - 1) * x$v[2]
          v <- v/df
          Args$Sigma <- lapply(v / x$n, as.matrix)
        }
        Args$group_parameters <- 1
        Args$joint_parameters <- 0
      }

      Bain_res <- do.call(bain, Args)
      Bain_res$call <- cl
      Bain_res$model <- x
      class(Bain_res) <- c("t_test", class(Bain_res))
      Bain_res
}

#' @method bain default
#' @export
bain.default <- function(x,
                         hypothesis,
                         fraction = 1,
                         ...,
                         n,
                         Sigma,
                         group_parameters = 0,
                         joint_parameters = 0
                         )
{

  cl <- match.call()
  Args <- as.list(cl[-1])
  n_estimates <- length(x)


# Parse hypotheses --------------------------------------------------------
  #ren_estimate <- rename_estimate(x)
  parsed_hyp <- parse_hypothesis(names(x), hypothesis)
  hyp_mat <- do.call(rbind, parsed_hyp$hyp_mat)
  n_hyp <- length(parsed_hyp$original_hypothesis)
  n_constraints <- parsed_hyp$n_constraints


# Check legal input -------------------------------------------------------
# CASPAR THE CHECK BELOW IS NO LONGER NECESSARY. ALL WORKS FINE
#    if(group_parameters > 1 & joint_parameters > 0){
#    stop("Bain can not yet evaluate hypotheses where group_parameters is larger than 1 and joint_parameters is larger than 0.")
#  }

  rank_hyp <- qr(hyp_mat)$rank

  ##for unit group
  if (group_parameters == 0) {
    if (length(n) != 1) {
      stop("Argument 'n' should be vector of length 1, with value equal to sample size, when 'group_parameters' = 0.")
    }
    if (is.list(Sigma)) {
      stop("Argument 'Sigma' should be a matrix or number when 'group_parameters' = 0.")
    }
    if (nrow(as.matrix(Sigma)) != n_estimates ||
        ncol(as.matrix(Sigma)) != n_estimates) {
      stop("The rank of covariance matrix 'Sigma' did not match the number of estimates in 'x'.")
    }
    if (checkcov(Sigma) == 1) {
      # CJ: Please give a more informative error here DONE-HH
      stop("Your covariance matrix ('Sigma') is not positive definite.")
    }

    b <- rank_hyp / (n / fraction)
    thetacovpost <- Sigma
    thetacovprior <- thetacovpost / b
  }

  ##for multiple groups
  if (group_parameters != 0) {
    #if(length(n)==1){stop("n should be a vector when group_parameters>0")}
    if (!is.list(Sigma)) {
      stop("Argument 'Sigma' should be a list of covariance matrices, with a number of elements equal to the number of 'group_parameters'.")
    }
    if (any(unlist(lapply(Sigma, checkcov)) == 1)) {
      # CJ: Please replace with a more informative error message
      stop("One of your covariance matrices ('Sigma') is not positive definite.")
    }

    dim_group_parameters <- sapply(Sigma, dim)
    if (any(dim_group_parameters != mean(dim_group_parameters))) {
      stop("Argument 'Sigma' should be a list of covariance matrices, and each covariance matrix should have the same dimensions.")
    }

    n_Sigma <- length(Sigma)
    if (n_Sigma != length(n)) {
      stop("Length of the vector of sample sizes is not equal to the number of covariance matrices in 'Sigma'.")
    }
    if (n_estimates != group_parameters * n_Sigma + joint_parameters) {
      stop("The length of the vector of estimates (parameter 'x') is not correct for multiple groups")
    }
    if (any(dim_group_parameters != group_parameters + joint_parameters)) {
      stop("The dimensions (rows and columns) of each covariance matrix in 'Sigma' should be equal to the number of group-specific parameters, plus the number of joint parameters ('group_parameters' + 'joint_parameters').")
    }

    b <- rep(0, n_Sigma)
    prior_cov <- vector("list", length = n_Sigma)
    for (p in 1:n_Sigma) {
      b[p] <- 1 / n_Sigma * rank_hyp / (n[p] / fraction)
      prior_cov[[p]] <- Sigma[[p]] / (b[p])
    }

    inv_prior <- lapply(prior_cov, solve)
    inv_post <- lapply(Sigma, solve)

    thetacovprior <- covmatrixfun(inv_prior, group_parameters, joint_parameters, n_Sigma)
    thetacovpost <- covmatrixfun(inv_post, group_parameters, joint_parameters, n_Sigma)
  }

# Check legality of constraints -------------------------------------------

  #check about equality constraints or the comparability issue
  About <- .Fortran(
    "about",
    numH = as.integer(n_hyp),
    numSP = as.integer(n_estimates),
    numR = as.integer(n_constraints),
    totalRr = hyp_mat,
    numARi = as.integer(rep(0, n_hyp)),
    error = as.integer(0)
  )

  hyp_matadjust <- About$totalRr
  numAR <- About$numARi
  error <- About$error

  if (qr(hyp_matadjust)$rank > (qr(hyp_matadjust[1:sum(n_constraints), 1:n_estimates])$rank + sum(numAR))) {
    error <- 2
  }

  for (i in 1:sum(n_constraints)) {
    for (j in 1:sum(n_constraints)) {
      if (all(hyp_matadjust[i, 1:n_estimates] == hyp_matadjust[j, 1:n_estimates]) &&
          abs(hyp_matadjust[i, n_estimates + 1] - hyp_matadjust[j, n_estimates +
                                                                1]) > 0) {
        error = 2
      }
      if (all(hyp_matadjust[i, 1:n_estimates] == -hyp_matadjust[j, 1:n_estimates]) &&
          abs(hyp_matadjust[i, n_estimates + 1] + hyp_matadjust[j, n_estimates +
                                                                1]) > 0) {
        error = 2
      }
    }
  }

  #Hypotheses are not comparable.
  if (error == 1) {
    stop(
      "Your hypotheses are not compatible, that is, they cannot be jointly
evaluated, OR, one of your hypotheses is impossible. See the vignette
      for an explanation of compatibility and possibility.
      "
    )
  }
  if (error == 2) {
    stop(
      "Your hypotheses are not compatible, that is, they cannot be jointly
evaluated, OR, one of your hypotheses is impossible. See the vignette
      for an explanation of compatibility and possibility.
      "
    )
  }


# End check legality of constraints ---------------------------------------

  fit <- com <- BF <- rep(0, n_hyp)
  fiteq <- fitin <- comeq <- comin <- rep(1, n_hyp)
  results <- rep(0, 5 * n_hyp)

# Start of a mega loop along the hypotheses -------------------------------

  for (h in 1:n_hyp) {
    ERr <- IRr <- constant <- 0
    if (n_constraints[2 * h - 1] != 0) {
      ERr <-
        matrix(hyp_mat[(sum(n_constraints[1:(2 * h - 1)]) - n_constraints[2 * h - 1] + 1):sum(n_constraints[1:(2 *
                                                                                              h - 1)]), 1:(n_estimates + 1)], n_constraints[2 * h - 1], n_estimates + 1)
    }
    if (n_constraints[2 * h] != 0) {
      IRr <-
        matrix(hyp_mat[(sum(n_constraints[1:(2 * h)]) - n_constraints[2 * h] + 1):sum(n_constraints[1:(2 *
                                                                                      h)]), 1:(n_estimates + 1)], n_constraints[2 * h], n_estimates + 1)
      constant = IRr[, n_estimates + 1]
    }

    #compute the rowrank of IRr and the linear combiniation of independent constraints
    Mrank <- .Fortran(
      "mrank",
      numIR = n_constraints[2 * h],
      numSP = n_estimates,
      rowrank = as.integer(0),
      IRr = IRr,
      transR = diag(0, n_constraints[2 * h], n_constraints[2 * h]),
      constant,
      transcon = rep(0, n_constraints[2 * h])
    )

    rowrank <- Mrank$rowrank
    IRr <- Mrank$IRr
    transR <- Mrank$transR
    transcon <- Mrank$transcon

    if (n_constraints[2 * h - 1] == 0) {
      Rr = IRr
    }
    if (n_constraints[2 * h] == 0) {
      Rr = ERr
    }
    if (n_constraints[2 * h - 1] != 0 && n_constraints[2 * h] != 0) {
      Rr = rbind(ERr, IRr)
    }

    #parameter transformation for the estimates of theta
    thetar <- c(x, -1)
    betapost <- Rr[1:(n_constraints[2 * h - 1] + rowrank), 1:(n_estimates + 1)] %*%
      thetar


    #parameter transformation for the covariance matrix of theta
    betacovpost <-
      Rr[1:(n_constraints[2 * h - 1] + rowrank), 1:n_estimates] %*% thetacovpost %*% t(matrix(Rr[1:(n_constraints[2 *
                                                                                                  h - 1] + rowrank), 1:n_estimates], nrow = n_constraints[2 * h - 1] + rowrank, ncol =
                                                                                       n_estimates))
    betacovpri <-
      Rr[1:(n_constraints[2 * h - 1] + rowrank), 1:n_estimates] %*% thetacovprior %*% t(matrix(Rr[1:(n_constraints[2 *
                                                                                                   h - 1] + rowrank), 1:n_estimates], nrow = n_constraints[2 * h - 1] + rowrank, ncol =
                                                                                        n_estimates))

    #specify prior mean
    betapri <- rep(0, n_constraints[2 * h - 1] + rowrank)

    #adjust prior mean for about equality constraints
    if (numAR[h] > 0) {
      for (i in (n_constraints[2 * h - 1] + 1):(n_constraints[2 * h - 1] + rowrank)) {
        for (j in (sum(n_constraints[1:(2 * h)]) - n_constraints[2 * h] + 1):sum(n_constraints[1:(2 * h)])) {
          if (all(Rr[i, 1:n_estimates] == hyp_matadjust[j, 1:n_estimates]) &&
              Rr[i, n_estimates + 1] != hyp_matadjust[j, n_estimates + 1])
          {
            betapri[i] = hyp_matadjust[j, n_estimates + 1] - Rr[i, n_estimates +
                                                                          1]
          }
        }
      }
    }
    invbetadiagpost <- tryCatch({
        diag(solve(as.matrix(betacovpost)))
      }, error = function(e) {
        stop(paste(e, "\nOne of the following issues caused an error. It could be that:\n* One or more of the constraints you specified is redundant. You have to delete one or more of the constraints without changing the hypothesis. For example, a = b & a > 0 & b > 0 is equivalent to a = b & a > 0\n* Your hypotheses are not compatible, that is, they cannot be jointly evaluated\n* One of your hypotheses is impossible. See the vignette for an explanation of compatibility and possibility.\n* Your covariance matrix is not positive definite, that is, it cannot exist and therefore contains errors. See the vignette for further explanations."), call. = FALSE)
      })
    invbetadiagpri <- diag(solve(as.matrix(betacovpri)))
    Bpost <-
      diag(1, n_constraints[2 * h - 1] + rowrank) - solve(diag(invbetadiagpost, n_constraints[2 *
                                                                              h - 1] + rowrank, n_constraints[2 * h - 1] + rowrank)) %*% solve(betacovpost)
    Bpri <-
      diag(1, n_constraints[2 * h - 1] + rowrank) - solve(diag(invbetadiagpri, n_constraints[2 *
                                                                             h - 1] + rowrank, n_constraints[2 * h - 1] + rowrank)) %*% solve(betacovpri)

    #equality constraints
    if (n_constraints[2 * h - 1] > 0) {
      fiteq[h] <-
        1 / sqrt((2 * pi) ^ n_constraints[2 * h - 1] * abs(det(as.matrix(betacovpost[1:n_constraints[2 *
                                                                                     h - 1], 1:n_constraints[2 * h - 1]])))) * exp(-1 / 2 * (betapost[1:n_constraints[2 * h - 1]] %*%
                                                                                                                                      solve(betacovpost[1:n_constraints[2 * h - 1], 1:n_constraints[2 * h - 1]]) %*% betapost[1:n_constraints[2 *
                                                                                                                                                                                                                     h - 1]]))
      comeq[h] <-
        1 / sqrt((2 * pi) ^ n_constraints[2 * h - 1] * abs(det(as.matrix(betacovpri[1:n_constraints[2 *
                                                                                    h - 1], 1:n_constraints[2 * h - 1]]))))
    }

    #inequality constraints
    if (n_constraints[2 * h] > 0) {
      # function for the computation of complexity or fit for inequality constraints
      fitcom <- function(bet, invbetadiag, B, seed) {
        forc = .Fortran(
          "forc",
          as.integer(n_constraints[2 * h - 1]),
          as.integer(n_constraints[2 * h]),
          as.integer(rowrank),
          bet,
          transcon,
          invbetadiag,
          B,
          transR,
          f_or_c = as.double(0),
          Numfc = as.integer(0),
          sample.int(1e10, 1)
        )
        return(c(forc$f_or_c, forc$Numfc))
      }
      forc_post <- .Fortran(
        "forc",
        as.integer(n_constraints[2 * h - 1]),
        as.integer(n_constraints[2 * h]),
        as.integer(rowrank),
        betapost,
        transcon,
        invbetadiagpost,
        Bpost,
        transR,
        f_or_c = as.double(0),
        Numfc = as.integer(0),
        sample.int(1e10, 1)
      )

      forc_prior <- .Fortran(
        "forc",
        as.integer(n_constraints[2 * h - 1]),
        as.integer(n_constraints[2 * h]),
        as.integer(rowrank),
        betapri,
        transcon,
        invbetadiagpri,
        Bpri,
        transR,
        f_or_c = as.double(0),
        Numfc = as.integer(0),
        sample.int(1e10, 1)
      )

      fitin[h] <- forc_post$f_or_c
      numf <- forc_post$Numfc
      comin[h] <- forc_prior$f_or_c
      numc <- forc_prior$Numfc
    }
  }


# End of mega loop --------------------------------------------------------

  #total fit and complexity
  fit <- fitin * fiteq
  com <- comin * comeq
  #return(list(fit, com, n_constraints,n_hyp))
  #Bayes factor for a hypothesis vs its complement
  BF <- fit/com
  no_eq <- n_constraints[seq(1, n_hyp*2, by = 2)] == 0
  BF[no_eq] <- BF[no_eq] / ((1 - fit[no_eq]) / (1 - com[no_eq]))

  # Create matrix of Bayes factors
  BFmatrix <- fit %*% t(1/fit) / com %*% t(1/com)
  rownames(BFmatrix) <- colnames(BFmatrix) <- paste0("H", 1:n_hyp)

  # Create table of fit indices
  res <- cbind("Fit_eq" = fiteq, "Com_eq" = comeq, "Fit_in" = fitin, "Com_in" = comin, "Fit" = fit, "Com" = com, "BF" = BF, "PMPa" = fit / com / sum(fit / com), "PMPb" = fit / com / (1 + sum(fit / com)), "BF.u" = fit / com, "BF.c" = BF)
  res <- rbind(res, NA)
  res[nrow(res), match("PMPb", colnames(res))] <- 1 / (1 + sum(fit / com))
  #res <- rbind(res, c(rep(NA, ncol(res)-1), 1 / (1 + sum(fit / com))))
  rownames(res) <- c(paste("H", 1:n_hyp, sep = ""), "Hu")
  # Either provide these as rownames, but can take a lot of space, or print a 'legend' below the table
  # rownames(res) <- parsed_hyp$original_hypothesis

  Bainres <- list(
    fit = data.frame(res),
    BFmatrix = BFmatrix,
    b = b,
    prior = thetacovprior,
    posterior = as.matrix(thetacovpost),
    call = cl,
    model = x,
    hypotheses = gsub("___X___", ":", parsed_hyp$original_hypothesis),
    independent_restrictions = rank_hyp,
    estimates = x,
    n = as.vector(n),
    Sigma = Sigma,
    group_parameters = group_parameters,
    joint_parameters = joint_parameters
  )
  class(Bainres) <- "bain"
  Bainres
}
