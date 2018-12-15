#' Bayes factors for informative hypotheses
#'
#' \code{bain} is an acronym for "Bayesian informative hypothesis evaluation". It uses the Bayes factor to
#' evaluate hypotheses specified using equality and inequality constraints among (linear combinations of)
#' parameters in a wide range of statistical models. An introduction is given in Hoijtink, Mulder, van Lissa,
#' and Gu (2018) retrievable from the Psychological Methods website \url{https://www.apa.org/pubs/journals/met/}
#' or the bain website \url{https://informative-hypotheses.sites.uu.nl/software/bain/}
#'
#' @param x An R object containing the outcome of a statistical analysis. Currently, the following
#' objects can be processed:
#' \itemize{
#' \item \code{lm()} objects (anova, ancova, multiple regression)
#' \item \code{t.test()} objects (Student's t-test, Welch's t-test, paired samples t-test, one-group t-test)
#' \item A named vector containing the estimates resulting from a statistical analysis.
#' Note that, named means that each estimate has to be labelled such that it can be referred to
#' in \code{hypotheses}.
#' }
#' @param hypotheses	A character string containing the informative hypotheses to evaluate (see Details).
#' @param ... Additional arguments (see Details).
#'
#' @details
#' ========== Using \code{bain} with an \link{lm} or \link{t.test} object ==========
#'
#' The following steps need to be executed:
#' \enumerate{
#' \item \code{x <- lm()} or \code{x <- t.test()}. Execute an analysis with \link{lm} or \link{t.test}.
#' See Examples for a complete elaboration of analyses that can and cannot be processed by \code{bain}.
#' \item \code{get_estimates(x)}.	Displays the estimates and the name attached to each estimate. These names are
#' used to specify \code{hypotheses}.
#' \item \code{label_estimates(x,labels)}. Note that, \code{labels} is a character vector containing new labels
#' for the estimates in \code{x}, in order to obtain meaningful and easy to use names. Note that, each name has
#' to start with a letter, and may consist of "letters", "numbers", ".", and "_".
#' \item \link{set.seed}\code{(seed)}. Set \code{seed} equal to an integer number to create a repeatable
#' random number sequence.
#' \item \code{bain(x,hypotheses)} or \code{bain(x,hypotheses,standardize = TRUE)}. The first call to \code{bain}
#' is used in case of \code{lm} implementations of anova, ancova, and t.test. The second call to \code{bain} is used in
#' case of \code{lm} implementations of multiple regression. With \code{standardize = TRUE} hypotheses with respect to
#' standardized regression coefficients are evaluated. With \code{standarize = FALSE} hypotheses with respect to
#' unstandardized regression coefficients are evaluated.
#' }
#'
#' ========== Using \code{bain} with a named vector ==========
#'
#' The following steps need to be executed:
#' \enumerate{
#' \item Execute a statistical analysis. Collect the estimates of interest in a vector. Assign names to the estimates
#' using \code{names(estimates)<-labels}. Note that, \code{labels} is a character vector containing new labels
#' for the estimates in \code{estimates}, in order to obtain meaningful and easy to use names. Note that,
#' each name has to start with a letter, and may consist of "letters", "numbers", ".", and "_".
#' \item \link{set.seed}\code{(seed)}. Set \code{seed} equal to an integer number to create a repeatable
#' random number sequence.
#' \item \code{bain(estimates,hypotheses,n=.,Sigma=.,group_parameters=0,joint_parameters=2)}. Execute \code{bain} with
#' arguments as elaborated next:
#' \itemize{
#' \item \code{estimates} A named vector with parameter estimates.
#' \item \code{hypotheses} A character string containing the informative hypotheses to evaluate (the specification
#' is elaborated in the next subsection).
#' \item \code{n} A vector containing the sample size of each group in the analysis. SOMS SINGLE NUMBER SOMS NIET
#' \item{Sigma} A list containing per group, the covariance matrix of the parameters (the size of this matrix is
#' groups+joint_parameters x groups+joint_parameters) SOMS LIST SOMS NIET
#' \item \code{group_parameters} In case of one group group_parameters = 0. In case of two or more groups,
#' the number of group specific parameters.
#' In, for example, an ANOVA with three group, and joint_parameters = 0,  est will contain three
#' parameters and group_parameters = 1
#' because each group is characterized by one mean. In, for example, an ANCOVA with  three groups and
#' two covariates, est will contain
#' five parameters (three adjusted means and the regression coefficients of two covariates),
#' group_parameters = 1 because each group is
#' characterized by one adjusted mean, and joint_parameters = 2 because there are two regression
#' coefficients that apply to each group.
#' \item \code{joint_parameters} In case of one group the number of parameters in est. In case of two or more groups, the number of parameters in est
#' shared by the groups.
#' }
#' }
#'
#' @return Returns a list that contains hypothesis testing results (i.e., Bayes
#' factors (BFs), relative fit (f), relative complexity (c), posterior model
#' probabilities (PMPs)), the approximated posterior and prior covariance
#' matrices, the approximated posteriors means, and the fraction (b).
#' @author Caspar van Lissa, Xin Gu, Herbert Hoijtink, Joris Mulder
#' @references Gu, X., Mulder, J., and Hoijtink, H. (2017). Approximated
#' adjusted fractional Bayes factors: A general method for testing informative
#' hypotheses. British Journal of Mathematical and Statistical Psychology.
#'
#' Hoijtink, H., Gu, X., and Mulder, J. (unpublished). Multiple group Bayesian
#' evaluation of informative hypotheses.
#' @keywords htest
#' @examples
#' \dontrun{
#' #One group:
#' #Example 1:
#' #Hypothesis
#' #H1: theta1>theta2>theta3   #coefficients in regression model
#'
#' #Input
#' estimate<-c(3,2,1)  #estimates of coefficients
#' Sigma<-matrix(c(3,0,0,0,2,0,0,0,1),3,3,byrow = TRUE) #covariance matrix of coefficients
#'
#' n<-50 #samples size
#'
#' #H1
#' ERr<-NULL
#' IRr<-matrix(c(1,-1,0,0,0,1,-1,0),nrow=2,ncol=4,byrow = TRUE)
#'
#' res<-Bain(estimate=estimate,grouppara=0,jointpara=3,Sigma=Sigma,n=n,ERr,IRr) #run
#' #Results are printed.
#' #Results for fit, complexity, Bayes factor, and PMPs are saved in "res":
#'
#' plot(res)
#' #Results for PMPs are plotted.
#'
#'
#' #Multiple groups
#' #Example 2
#' #t test:
#' #Hypotheses:
#' #H1: theta1=theta2   #group means
#' #H2: theta1>theta2
#' #H3: theta1<theta2
#'
#' #Input
#' estimate<-c(0,0)         #Estimates of group means theta1 and theta2.
#'
#' cov1<-matrix(c(.5),1,1)
#' cov2<-matrix(c(.1),1,1)
#' Sigma<-list(cov1,cov2) #List of variances of group means
#'
#' n<-c(22,37)           #samplesize
#'
#' #H1:
#' ERr1<-matrix(c(1,-1,0),nrow=1,ncol=3,byrow = TRUE)
#' IRr1<-NULL
#'
#' #H2
#' ERr2<-NULL
#' IRr2<-matrix(c(1,-1,0),nrow=1,ncol=3,byrow = TRUE)
#'
#' #H3
#' ERr3<-NULL
#' IRr3<-matrix(c(-1,1,0),nrow=1,ncol=3,byrow = TRUE)
#'
#' res<-Bain(estimate,Sigma,grouppara=1,jointpara=0,n=n,ERr1,IRr1,ERr2,IRr2,ERr3,IRr3) #run
#' #Results are printed.
#' #Results for fit, complexity, Bayes factor, and PMPs are also saved in "res":
#'
#' plot(res)
#' #Results for PMPs are plotted.
#' }
#' @rdname bain
#' @export
#' @useDynLib bain, .registration = TRUE
#' @importFrom stats as.formula coef complete.cases cov lm model.frame
#' model.matrix pt qt sd setNames summary.lm var vcov
#'
bain <- function(x, hypothesis, ...) {
  UseMethod("bain", x)
}


#' @method bain lm
#' @export
bain.lm <-
  function(x,
           hypothesis,
           ...,
           standardize = FALSE) {

    cl <- match.call()
    Args <- as.list(cl[-1])
    # Checken of het factor OF ordered factor is!!!!!!
    # Nu wordt overal (?) factor_variables[-1] gebruikt. Kan het niet gewoon één keer hier [-1]?
    factor_variables <- sapply(x$model[-1], inherits, what = "factor")
    Warnings <- NULL
    # Checken of het factor OF ordered factor is!!!!!!
    which_model <- if(any(factor_variables)){
        if(ncol(x$model) == 2){
          "ANOVA"
        } else {
          if(sum(factor_variables) == 1){
            "ANCOVA"
          } else {
            "mixed_predictors"
            Warnings <- c(Warnings,
                          "Calling bain on an object of type 'lm' with mixed predictors (factors and numeric predictors) may result in untrustworthy results. Please interpret with caution."
            )
          }
        }
      } else {
        "continuous_predictors"
      }

    switch(which_model,
           ANOVA = {
             if(names(x$coefficients)[1] == "(Intercept)"){
               Warnings <- c(Warnings, "Your ANOVA model included an intercept, which was dropped by bain. Please specify your hypothesis in terms of group means.")
             }
             n <- table(x$model[, 2])
             anovafm <- lm(x$model[, 1] ~ -1 + x$model[, 2])
             Args$x <- anovafm$coefficients
             names(Args$x) <- levels(x$model[, 2])
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
             ancovafm <-  lm(as.formula(paste0(names(df)[1], "~ -1 + .")), df)

             Args$x <- ancovafm$coefficients
             names(Args$x) <- gsub(names(df)[var_factor], "", names(Args$x))
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

             predictor_names <- names(x$coefficients) # If (Intercept) must be dropped, drop it BY NAME, don't drop first column

             if(any(factor_variables)){
               predictor_names <- gsub(paste0("^(",
                                              paste(names(x$model)[-1][factor_variables], sep = "|"),
                                              ")"), "", predictor_names)
             }

             covariates_hypo <-
               predictor_names[sapply(predictor_names, grepl, x = hypothesis)]

             if (!standardize) {
               estimate <- coef(x)[covariates_hypo]
               Sigma <- vcov(x)[covariates_hypo, covariates_hypo]
             } else{
               # Hier moeten even de juiste namen meegegeven worden!!!
               ses <- seBeta(
                 x$model[, -1],
                 x$model[, 1],
                 Nobs = nrow(x$model),
                 alpha = .05,
                 estimator = 'Normal'
               )
               select_parameters <- which(names(x$model)[-1] %in% covariates_hypo)
               estimate <- ses$CIs$estimate[select_parameters]
               # Check even of dit lekker loopt!
               names(estimate) <- names(x$model)[-1][select_parameters]
               Sigma <- ses$cov.mat[select_parameters, select_parameters]
             }

             Args$x <- estimate
             Args$Sigma <- Sigma
             Args$group_parameters <- 0
             Args$joint_parameters <- length(covariates_hypo)
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
    Bain_res
  }


#' @method bain htest
#' @export
bain.htest <-
  function(x,
           hypothesis,
           ...) {
    stop("To be able to run bain on the results of an object returned by t.test(), you must first load the 'bain' package, and then conduct your t.test. The standard t.test does not return group-specific variances and sample sizes, which are required by bain. When you load the bain package, the standard t.test is replaced by a version that does return this necessary information.")
}

#' @method bain bain_htest
#' @export
bain.bain_htest <-
  function(x,
           hypothesis,
           ...) {
      cl <- match.call()
      Args <- as.list(cl[-1])

      Args$x <- x$estimate
      Args$n <- x$n

      if(length(x$estimate) == 1){
        Args$Sigma <- x$v/x$n
        Args$group_parameters <- 0
        Args$joint_parameters <- 1
      } else {
        if (!x$method == " Two Sample t-test") {
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
      class(Bain_res) <- c("bain_htest", class(Bain_res))
      Bain_res
}

#' @method bain default
#' @export
bain.default <- function(x,
                         hypothesis,
                         ...,
                         n,
                         Sigma,
                         group_parameters = 0,
                         joint_parameters = 0
                         )
{

  cl <- match.call()
  Args <- as.list(cl[-1])
  seed <- sample(1:2^15, 1)

  estimate <- rename_estimate(x)
  n_estimates <- length(estimate)


# Parse hypotheses --------------------------------------------------------

  parsed_hyp <- parse_hypothesis(names(estimate), hypothesis)
  hyp_mat <- parsed_hyp$hyp_mat
  n_hyp <- length(parsed_hyp$original_hypothesis)
  n_constraints <- parsed_hyp$n_constraints


# Check legal input -------------------------------------------------------

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
      # CJ: Please give a more informative error here
      stop("the covariance matrix 'Sigma' you entered contains errors since it cannot exist")
    }

    b <- rank_hyp / n
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
      stop("the covariance matrix 'Sigma' you entered contains errors since it cannot exist")
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
      b[p] <- 1 / n_Sigma * rank_hyp / n[p]
      prior_cov[[p]] <- Sigma[[p]] / b[p]
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
      "BaIn is not suited for the evaluation of one or more of the hypotheses,\n because the adjusted prior mean cannot be determined from R theta = r."
    )
  }
  if (error == 2) {
    stop(
      "The informative hypotheses under evaluation are not comparable,\n and/or BaIn is not suited for the evaluation of one or more of the hypotheses,\n because the adjusted prior mean cannot be determined from R theta = r."
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
    thetar <- c(estimate, -1)
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

    invbetadiagpost <- diag(solve(as.matrix(betacovpost)))
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
          as.integer(seed)
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
        as.integer(seed)
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
        as.integer(seed)
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
  res <- cbind("Fit_eq" = fiteq, "Com_eq" = comeq, "Fit_in" = fitin, "Com_in" = comin, "Fit" = fit, "Com" = com, "BF" = BF, "PMPa" = fit / com / sum(fit / com), "PMPb" = fit / com / (1 + sum(fit / com)))
  res <- rbind(res, c(rep(NA, ncol(res)-1), 1 / (1 + sum(fit / com))))
  rownames(res) <- c(paste("H", 1:n_hyp, sep = ""), "Hu")
  # Either provide these as rownames, but can take a lot of space, or print a 'legend' below the table
  # rownames(res) <- parsed_hyp$original_hypothesis

  Bainres <- list(
    fit = data.frame(res),
    BFmatrix = BFmatrix,
    b = b,
    prior = thetacovprior,
    posterior = thetacovpost,
    call = cl,
    model = x,
    hypotheses = parsed_hyp$original_hypothesis,
    independent_restrictions = rank_hyp,
    estimates = x,
    n = n
  )
  class(Bainres) <- "bain"
  Bainres
}
