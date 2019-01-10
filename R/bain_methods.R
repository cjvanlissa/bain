#' Bayes factors for informative hypotheses
#'
#' \code{bain} is an acronym for "Bayesian informative hypotheses evaluation".
#' It uses the Bayes factor to evaluate hypotheses specified using equality and
#' inequality constraints among (linear combinations of) parameters in a wide
#' range of statistical models. A tutorial is provided by Hoijtink, Mulder,
#' van Lissa, and Gu (2018) retrievable from the Psychological Methods website
#' at \url{https://www.apa.org/pubs/journals/met/} or the bain website at
#' \url{https://informative-hypotheses.sites.uu.nl/software/bain/} Users are
#' advised to read this tutorial before using \code{bain}.
#'
#' @param x An R object containing the outcome of a statistical analysis.
#' Currently, the following objects can be processed:
#' \itemize{
#' \item \code{lm()} objects (anova, ancova, multiple regression). In the
#' Example section it is elaborated which calls to \code{lm} can be processed by
#' \code{bain} (other calls cannot be processed).
#' \item \code{t.test()} objects (Student's t-test, Welch's t-test, paired
#' samples t-test, one-group t-test, equivalence test).
#' \item A named vector containing the estimates resulting from a statistical
#' analysis.
#' Note that, named means that each estimate has to be labelled such that it can
#' be referred to in \code{hypotheses}.
#' }
#' @param hypothesis	A character string containing the informative hypotheses
#' to evaluate (see Details).
#' @param ... Additional arguments (see Details).
#'
#' @details
#' ========== Using \code{bain} with an \link{lm} or \link{t.test} object ======
#'
#' The following steps need to be executed:
#' \enumerate{
#' \item \code{x <- lm()} or \code{x <- t.test()}. Execute an analysis with
#' \link{lm} or \link{t.test}. See Examples for a complete elaboration of the
#' calls to \code{lm} and \code{t.test} that can be processed by \code{bain}.
#' Note that, \code{lm} and \code{t.test} will apply list-wise deletion if there
#' are cases with missing values in the variables used.
#' \item Displays the estimates such that in the next step new names can be
#' given to the  estimates.
#' These names are used to specify \code{hypotheses}. For \code{lm()},
#' \code{coef(x)} will display the
#' estimates. For \code{t.test()}, \code{print(x)} will display the estimates.
#' \item \code{label_estimates(x,labels)}. Note that, \code{labels} is a
#' character vector containing new labels for the estimates in \code{x}. Each
#' label has to start with a letter, and may consist of "letters", "numbers",
#' ".", and "_". An example is \code{labels <- c("a", "b", "c")}.
#' \item \link{set.seed}\code{(seed)}. Set \code{seed} equal to an integer
#' number to create a repeatable random number sequence.
#' \item \code{results <- bain(x,hypotheses)} or \code{results <-
#' bain(x,hypotheses,standardize = TRUE)}. The first call to \code{bain} is used
#' in case of \code{lm} implementations of anova, ancova, and \code{t.test}. The
#' second call to \code{bain} is used in case of \code{lm} implementations of
#' multiple regression. With \code{standardize = TRUE} hypotheses with respect
#' to standardized regression coefficients are evaluated. With \code{standarize
#' = FALSE} hypotheses with respect to unstandardized regression coefficients
#' are evaluated.
#' \item \code{print(results)} Print the results of an analysis wity
#' \code{bain}.
#' \item \code{descriptives(results, ci=0.95)} Present descriptives for the
#' parameters used to specify \code{hypotheses}.
#' }
#'
#' ========== Using \code{bain} with a named vector ==========
#'
#' The following steps need to be executed:
#' \enumerate{
#' \item Execute a statistical analysis. In case of a single group analysis, the
#' following information has to be extracted from the statistical analysis and
#' supplied to \code{bain}: 1) a vector containing estimates of the parameters
#' used to specify \code{hypotheses}; 2) a list containing the covariance matrix
#' of these parameters; and, 3) the sample size. In case of a multiple group
#' analysis, the following information has to be extracted from the statistical
#' analysis and supplied to \code{bain}: 1) a vector containing estimates of the
#' parameters used to specify \code{hypotheses} possibly augmented with the
#' estimates of other parameters; a list containing per group the covariance
#' matrix of the parameters corresponding to the group at hand and, possibly,
#' the augmented parameters; and, per group the sample size.
#' \item Assign names to the estimates
#' using \code{names(estimates)<-labels}. Note that, \code{labels} is a
#' character vector containing new labels for the estimates in \code{estimates}.
#' Each label has to start with a letter, and may consist of "letters",
#' "numbers", ".", and "_". These labels are used to specify \code{hypotheses}
#' (see below). An example is \code{labels <- c("a", "b", "c")}.
#' \item \link{set.seed}\code{(seed)}. Set \code{seed} equal to an integer
#' number to create a repeatable random number sequence.
#' \item \code{results <- bain(estimates, hypotheses, n=., Sigma=.,
#' group_parameters = 2, joint_parameters = 0)} executes \code{bain} with the
#' followiong arguments:
#' \itemize{
#' \item \code{estimates} A named vector with parameter estimates.
#' \item \code{hypotheses} A character string containing the informative
#' hypotheses to evaluate (the specification is elaborated below).
#' \item \code{n} A vector containing the sample size of each group in the
#' analysis. See, Hoijtink, Gu, and Mulder (2018), for an elaboration of the
#' difference between one and multiple group analyses. A multiple group
#' analysis is required when group specific parameters are used to formulate
#' \code{hypotheses}. Examples are the Student's and Welch's t-test, ANOVA, and
#' ANCOVA. See the Examples section for elaborations of the specification of
#' multiple group analyses when a named vector is input for \code{bain}.
#' \item \code{Sigma} A list of covariance matrices. In case of one group
#' analyses the list contains one covariance matrix. In case of multiple group
#' analyses the list contains one covariance matrix for each group. See the
#' Example section and Hoijtink, Gu, and Mulder (2018) for further instructions.
#' \item \code{group_parameters} The number of group specific parameters.  In,
#' for example, an ANOVA with three groups, \code{estimates} will contain three
#' sample means, \code{group_parameters = 1} because each group is characterized
#' by one mean, and \code{joint_parameters = 0} because there are no parameters
#' that apply to each of the groups.. In, for example, an ANCOVA with  three
#' groups and two covariates, \code{estimates} will contain five parameters
#' (three adjusted means and the regression coefficients of two covariates),
#' \code{group_parameters = 1} because each group is characterized by one
#' adjusted mean, and \code{joint_parameters = 2} because there are two
#' regression coefficients that apply to each group. In, for example, a repeated
#' measures design with four repeated measures and two groups (a between factor
#' with two levels and a  within factor with four levels) \code{estimates} will
#' contain eight means (four for each group), \code{group_parameters = 4}
#' because each group is characterized by four means and \code{joint_parameters
#' = 0} because there are no parameters that apply to each of the groups.
#' \item \code{joint_parameters} In case of one group \code{joint_parameters =
#' 0}. In case of two or more groups, the number of parameters in
#' \code{estimates} shared by the groups. In, for example, an ANCOVA, the number
#' of \code{joint_parameters} equals the number of covariates.
#' }
#' \item \code{print(results)} Print the results of an analysis wity
#' \code{bain}.
#' \item \code{descriptives(results, ci=0.95)} Present descriptives for the
#' parameters used to specify \code{hypotheses}.
#' }
#'
#' ========== The specification of \code{hypotheses} ==========
#'
#' \code{hypotheses} is a character string that specifies which informative
#' hypotheses have to be evaluated. A simple example is \code{hypotheses <- "a >
#' b > c; a = b = c;"} which specifies two hypotheses using three estimates with
#' names "a", "b", and "c", respectively.
#'
#' The hypotheses specified have to adhere to the following rules:
#' \enumerate{
#' \item Parameters are referred to using the names specified in
#' \code{label_estimates()} or \code{names()}.
#' \item Linear combinations of parameters must be specified adhering to the
#' following rules:
#'         \enumerate{ \item Each parameter name is used at most once.
#'                     \item Each parameter name may or may not be
#'                     pre-multiplied with a number.
#'                     \item A constant may be added or subtracted from each
#'                     parameter name.
#'                     \item A linear combination can also be a single number.}
#' Examples are: \code{3 * a + 5}; \code{a + 2 * b + 3 * c - 2}; \code{a - b};
#' and \code{5}.
#' \item (Linear combinations of) parameters can be constrained using <, >, and
#' =. For example, \code{a > 0} or
#' \code{a > b = 0} or \code{2 * a < b + c > 5}.
#' \item The ampersand & can be used to combine different parts of a hypothesis.
#' For example, \code{a > b & b > c} which is equivalent to \code{a > b > c} or
#' \code{a > 0 & b > 0 & c > 0}.
#' \item Sets of (linear combinations of) parameters subjected to the same
#' constraints can be specified using (). For
#' example, \code{a > (b,c)} which is equivalent to \code{a > b & a > c}.
#' \item The specification of a hypothesis is completed by typing ; For example,
#' \code{hypotheses <- "a > b > c; a = b = c;"}, specifies two hypotheses.
#' \item Hypotheses have to be compatible, non-redundant and possible. What
#' these terms mean will be elaborated below.
#' }
#'
#' \emph{The set of hypotheses has to be compatible}. For the statistical
#' background of this requirement see Gu, Mulder, Hoijtink (2018). Usually the
#' sets of hypotheses specified by researchers are compatible, and if not,
#' \code{bain} will return an error message. The following steps can be used to
#' determine if a set of hypotheses is compatible:
#' \enumerate{
#' \item	Replace a range constraint, e.g., \code{1 < a1 < 3}, by an equality
#' constraint in which the parameter involved is equated to the midpoint of the
#' range, that is, \code{a1 = 2}.
#' \item Replace in each hypothesis the < and > by =. For example, \code{a1 = a2
#' > a3 > a4} becomes \code{a1 = a2 = a3 = a4}.
#' \item The hypotheses are compatible if there is at least one solution to the
#' resulting set of equations. For the two hypotheses considered under 1. and
#' 2., the solution is a1 = a2 = a3 = a4 = 2. An example of two non-compatible
#' hypotheses is \code{hypotheses <- "a = 0; a > 2;"} because there is no
#' solution to the equations \code{a=0} and \code{a=2}.
#' }
#'
#' \emph{Each hypothesis in a set of hypotheses has to be non-redundant.} A
#' hypothesis is redundant if it can also be specified with fewer constraints.
#' For example, \code{a = b & a > 0 & b > 0} is redundant because it can also be
#' specified as \code{a = b & a > 0}. \code{bain} will work correctly if
#' hypotheses specified using only < and > are redundant. \code{bain} will
#' return an error message if hypotheses specified using at least one = are
#' redundant.
#'
#' \emph{Each hypothesis in a set of hypotheses has to be possible.} An
#' hypothesis is impossible if estimates in agreement with the hypothesis do not
#' exist. For example: values for \code{a} in agreement with \code{a = 0 &
#' a > 2} do not exist. It is the responsibility of the user to ensure that the
#' hypotheses specified are possible. If not, \code{bain} will either return an
#' error message or render an output table containing \code{Inf}'s.
#'
#' @return The commands \code{bain()} or \code{results<-bain()} followed by
#' \code{results} or \code{print(results)} will render the default (most
#' important) output from \code{bain}. These concern for each hypothesis
#' specified in \code{hypothesis} the fit, complexity, Bayes factor versus its
#' complement, posterior model probability (based on equal prior model
#' probabilities) excluding the unconstrained hypothesis, and posterior model
#' probability including the unconstrained hypothesis. In Hoijtink, Mulder,
#' van Lissa, and Gu (2018) it is elaborated how these quantities (and the outer
#' output presented below) should be interpreted. Additionally, using
#' \code{descriptives(results, ci=0.95)}, a descriptives matrix can be obtained
#' in which for each estimate, the label, the value, and a 95\% central
#' credibility interval is presented. The following commands can be used to
#' retrieve the default and additional information from the \code{bain} output
#' object:
#' \enumerate{
#' \item \code{results$fit} renders the default output, \code{results$fit$Fit}
#' contains only the column containing the fit of each hypothesis. In the last
#' command \code{Fit} can be replaced by \code{Com}, \code{BF}, \code{PMPa},
#' \code{PMPb} to obtain the information in the corresponding columns of the
#' default output.
#' \item \code{results$BFmatrix} contains the matrix containing the mutual Bayes
#' factors of the hypotheses specified in \code{hypotheses}.
#' \item \code{results$b} contains for each of the groups in the analysis the
#' fraction of information of the data in the group at hand used to specify the
#' covariance matrix of the prior distribution.
#' \item \code{results$prior} contains the covariance matrix of the prior
#' distribution.
#' \item \code{results$posterior} contains the covariance matrix of the
#' posterior distribution.
#' \item \code{results$call} displays the call to \code{bain}.
#' \item \code{results$model} displays the named vector or the call to the R
#' function rendering the R object that is input to \code{bain}.
#' \item \code{results$n} displays the sample sizes per group.
#' \item \code{results$independent_restrictions} displays the number of
#' independent constraints in the set of hypotheses under consideration.
#' \item \code{results$fit$Fit_eq} displays the fit of the equality constrained
#' part of each hypothesis. Replacing \code{Fit_eq} by \code{Fit_in}, renders
#' the fit of the inequality constrained part of an hypothesis conditional on
#' the fit of the equality constrained part. \code{Com_eq}, and \code{Com_in},
#' repectively, are the complexity counterparts of \code{Fit_eq}, and
#' \code{Fit_in}.
#' }
#'
#' @author The main authors of the bain package are Xin Gu (computational core
#' of the code, statistical underpinings), Caspar van Lissa (creation of CRAN
#' package, user interface and maintainer), Herbert Hoijtink (statistical
#' underpinnings) and Joris Mulder (statistical underpinnings). Contributions
#' were made by Marlyne Bosman (robust Bayes factors) and Camiel van Zundert
#' (\code{bain} for structural equation models). Contact information can be
#' found on the bain website at
#' \url{https://informative-hypotheses.sites.uu.nl/software/bain/}
#'
#' @references All references are retrievable via or from
#' \url{https://informative-hypotheses.sites.uu.nl/software/bain/}
#'
#' Bosman, M. and Hoijtink, H. (unpublished). Robust Bayes factors for Bayesian
#' Anova: overcoming overcoming adverse effect of non-normality and outliers.
#'
#' Gu, X., Mulder, J., and Hoijtink, H. (2018). Approximate adjusted fractional
#' Bayes factors: A general method for testing informative hypotheses.
#' \emph{British Journal of Mathematical and Statistical Psychology, 71,}
#' 229-261. DOI: 10.1111/bmsp.12110
#'
#' Gu, X., Hoijtink, H., Mulder, J., and Rosseel, Y. (unpublished). Bain: A
#' program for Bayesian testing of order constrained hypotheses in structural
#' equation models.
#'
#' Hoijtink, H., Mulder, J., van Lissa, C., and Gu, X. (2018). A tutorial on
#' testing hypotheses using the Bayes factor. \emph{Psychological Methods.}
#' DOI: 10.1037/met0000201
#'
#' Hoijtink, H., Gu, X., and Mulder, J. (2018). Bayesian evaluation of
#' informative hypotheses for multiple populations. \emph{British Journal of
#' Mathematical and Statistical Psychology.} DOI: 10.1111/bmsp.12145
#'
#' Hoijtink, H., Gu, X., Mulder, J., and Rosseel, Y. (in press). Computing Bayes
#' Factors from Data with Missing Values. \emph{Psychological Methods.}
#' DOI: 10.1037/met0000187
#'
#' @seealso The bain website at
#' \url{https://informative-hypotheses.sites.uu.nl/software/bain/} contains
#' downloadable legacy versions of bain (Bain-0.0.1 until Bain-0.1.2).
#' Furthermore, it contains contact information, messages, and announcements of
#' upcoming workshops. A part of bain (Bayesian t-tests, ANOVA, ANCOVA, and
#' multiple regression) is implemented in JASP \url{http://jasp-stats.org/}.
#' JASP provides a user-friendly interface and is suited for students and
#' researchers that are not familiar with the R package.
#'
#' @examples
#'
#' \dontrun{
#' # Note that, each of the examples given below can be run by copy pasting them
#' # into the Source screen of RStudio. An Examples.R file can also be
#' # downloaded from the baij website at
#' # \url{https://informative-hypotheses.sites.uu.nl/software/bain/}
#'
#' # ==============================================================
#' # Example Data Set Build into bain
#' # ==============================================================
#'
#' # Unless indicated otherwise, the examples that follow below use a simulated
#' # data set inspired by the Sesame Street data set from:
#' # Stevens, J. P. (1996). Applied Multivariate Statistics for the Social
#' # Sciences. Mahwah NJ: Lawrence Erlbaum. This data set is included in the
#' # bain package. The variables contained in sesamesim.txt are subsequently:
#'
#' # + sex (1 = boy, 2 = girl) of the child
#' # + site (1 = disadvantaged inner city, 2 = advantaged suburban , 3 =
#' #   advantaged rural,
#' #   4 = disadvantaged rural, 5 = disadvantaged Spanish speaking) from which
#' #   the child originates
#' # + setting (1 = at home, 2 = at school) in which the child watches sesame
#' #   street
#' # + age (in months) of the child
#' # + viewenc (0 = no, 1 = yes), whether or not the child is encouraged to
#' #   watch Sesame Street
#' # + peabody (mental age) score of the child (higher score is higher mental
#' #   age)
#' # + prenumb (score on a numbers test before watching Sesame Street for a
#' #   year)
#' # + postnumb (score on a numbers test after watching Sesame Street for a
#' #   year)
#' # + funumb (follow up numbers test score measured one year after postnumb).
#'
#' # =================================================================
#' # The examples that follow below are organized in three categories:
#' # Running bain with a t.test object; running bain with a lm object;
#' # and, running bain with a named vector. The ANOVA and ANCOVA
#' # examples are provided using both an lm object and a named vector
#' # as input for bain. Below you will find the following examples:
#' # =================================================================
#' #
#' # EXAMPLES USING A T.TEST OBJECT
#' # + BayesianStudent's t-test (equal within group variances)
#' # + Bayesian Welch's t-test (unequal within group variances
#' # + Bayesian paired samples t-test
#' # + Bayesian one group t-test
#' # + Bayesian Equivalence test
#' #
#' # EXAMPLES USING A LM OBJECT
#' # Note that, ONLY the type of calls to LM presented in the examples can be
#' # processed by bain. Calls structured in a different manner will either
#' # render an error message, or results that are meaningless.
#' # + Bayesian ANOVA. The example concerns a one-way ANOVA. Two-way or higher
#' #   order ANOVA's can only be handled by recoding all factors into one
#' #   factor. If, for example, there is a factor sex with levels man and woman,
#' #   and a factor age with levels young and old, these have to be recoded in a
#' #   new factor sexage with levels manyoung, manold, womanyoung, womanold
#' # + Bayesian ANCOVA. The example concerns a one-way ANCOVA. Two-way or higher
#' #   order ANCOVA's can only be handled by recoding all factors into one
#' #   factor.
#' # + Bayesian multiple regression
#' #
#' # EXAMPLES USING A NAMED VECTOR
#' # + Bayesian ANOVA
#' # + Bayesian ANCOVA
#' # + Bayesian repeated measures analysis (one within factor)
#' # + Bayesian repeated measures analysis (within between design)
#' # + Bayesian one group logistic regression (counterpart of multiple
#' #   regression)
#' # + Bayesian multiple group logistic regression (counterpart of ANCOVA)
#' # + Bayesian robust ANOVA (unequal within groups variances)
#' # + Bayesian multiple regression with missing data
#' # + Bayesian structural equation modelling
#'
#' # ADDITIONAL EXAMPLES CONTAINED IN BFTutorial.R DOWNLOADABLE FROM
#' # the bain website at
#' # \url{https://informative-hypotheses.sites.uu.nl/software/bain/}
#' # + Executing a sensitivity analysis with bain
#' # + Executing Bayesian updating with bain
#'
#' # ===========================================================================
#' # USING BAIN WITH A T.TEST OBJECT: EXAMPLE I
#' # An example of the Bayesian Student's t-test (equal within group variances)
#' # ===========================================================================
#' # load the bain package which includes the simulated sesamesim.txt data set
#' library(bain)
#' # collect the data for the boys in the vector x and for the girs in the
#' # vector y
#' x<-sesamesim$postnumb[which(sesamesim$sex==1)]
#' y<-sesamesim$postnumb[which(sesamesim$sex==2)]
#' # execute student's t-test
#' ttest <- t.test(x,y,paired = FALSE, var.equal = TRUE)
#' # inspect the estimates of the group means
#' print(ttest)
#' # assign names to the estimates
#' ttest <- label_estimates(ttest, c("boy","girl"))
#' # set a seed value
#' set.seed(100)
#' # test hypotheses with bain
#' results <- bain(ttest, "boy = girl; boy > girl; boy < girl")
#' # display the results
#' print(results)
#' # obtain the descriptives table
#' descriptives(results, ci = 0.95)
#' #
#' #
#' # ===========================================================================
#' # USING BAIN WITH A T.TEST OBJECT: EXAMPLE II
#' # An example of the Bayesian Welch's t-test (unequal within group variances)
#' # ===========================================================================
#' # load the bain package which includes the simulated sesamesim.txt data set
#' library(bain)
#' # collect the data for the boys in the vector x and for the girs in the
#' # vector y
#' x<-sesamesim$postnumb[which(sesamesim$sex==1)]
#' y<-sesamesim$postnumb[which(sesamesim$sex==2)]
#' # execute student's t-test
#' ttest <- t.test(x,y,paired = FALSE, var.equal = FALSE)
#' # inspect the estimates of the group means
#' print(ttest)
#' # assign names to the estimates
#' ttest <- label_estimates(ttest, c("boy","girl"))
#' # set a seed value
#' set.seed(100)
#' # test hypotheses with bain
#' results <- bain(ttest, "boy = girl; boy > girl; boy < girl")
#' # display the results
#' print(results)
#' # obtain the descriptives table
#' descriptives(results, ci = 0.95)
#' #
#' #
#' # ===========================================================================
#' # USING BAIN WITH A T.TEST OBJECT: EXAMPLE III
#' # An example of the Bayesian paired samples t-test
#' # ===========================================================================
#' # load the bain package which includes the simulated sesamesim.txt data set
#' library(bain)
#' # compare the pre with the post measurements
#' ttest <- t.test(sesamesim$prenumb,sesamesim$postnumb,paired = TRUE)
#' # inspect the estimates of the group means
#' print(ttest)
#' # assign names to the estimates
#' ttest <- label_estimates(ttest, c("difference"))
#' # set a seed value
#' set.seed(100)
#' # test hypotheses with bain
#' results <- bain(ttest, "difference=0; difference>0; difference<0")
#' # display the results
#' print(results)
#' # obtain the descriptives table
#' descriptives(results, ci = 0.95)
#' #
#'
#' # ===========================================================================
#' # USING BAIN WITH A T.TEST OBJECT: EXAMPLE IV
#' # An example of the Bayesian one group t-test
#' # ===========================================================================
#' # load the bain package which includes the simulated sesamesim.txt data se
#' library(bain)
#' # compare post measurements with the reference value 30
#' ttest <- t.test(sesamesim$postnumb)
#' # inspect the estimate of the mean
#' print(ttest)
#' # assign a name to the estimate
#' ttest <- label_estimates(ttest, c("post"))
#' # set a seed value
#' set.seed(100)
#' # test hypotheses with bain versus the reference value 30
#' results <- bain(ttest, "post=30; post>30; post<30")
#' # display the results
#' print(results)
#' # obtain the descriptives table
#' descriptives(results, ci = 0.95)
#' #
#' #
#' # ===========================================================================
#' # USING BAIN WITH A T.TEST OBJECT: EXAMPLE V
#' # An example of the Bayesian equivalence test
#' # ===========================================================================
#' # load the bain package which includes the simulated sesamesim.txt data set
#' library(bain)
#' # collect the data for the boys in the vector x and for the girs in the
#' # vector y
#' x<-sesamesim$postnumb[which(sesamesim$sex==1)]
#' y<-sesamesim$postnumb[which(sesamesim$sex==2)]
#' # execute student's t-test
#' ttest <- t.test(x,y,paired = FALSE, var.equal = TRUE)
#' # inspect the estimates of the group means
#' print(ttest)
#' # compute the pooled within standard deviation using the variance of x
#' # (ttest$v[1]) and y (ttest$v[2])
#' pwsd <- sqrt(((length(x) -1) * ttest$v[1] + (length(y)-1) * ttest$v[2])/
#' ((length(x) -1) + (length(y) -1)))
#' # print pwsd in order to be able to include it in the hypothesis. Its value
#' # is 12.60
#' print(pwsd)
#' # assign names to the estimates
#' ttest <- label_estimates(ttest, c("boy","girl"))
#' # set a seed value
#' set.seed(100)
#' # test hypotheses (the means of boy and girl differ less than .2 * pwsd =
#' # 2.52 VERSUS the means differ more than .2 * pwsd = 2.52) with bain
#' # note that, .2 is a value for Cohen's d reflecting a "small" effect, that
#' # is, the means differ less or more than .2 pwsd.
#' results <- bain(ttest, "boy - girl > -2.52 & boy - girl < 2.52")
#' # display the results
#' print(results)
#' # obtain the descriptives table
#' descriptives(results, ci = 0.95)
#' #
#' #
#' # ===========================================================================
#' # USING BAIN WITH A LM OBJECT: EXAMPLE I
#' # An example of the Bayesian ANOVA
#' # ===========================================================================
#' # load the bain package which includes the simulated sesamesim.txt data set
#' library(bain)
#' # make a factor of variable site
#' sesamesim$site <- as.factor(sesamesim$site)
#' # execute an analysis of variance using lm() which, due to the -1, returns
#' # estimates of the means per group
#' anov <- lm(postnumb~site-1,sesamesim)
#' # take a look at the estimated means and their names
#' coef(anov)
#' # choose convenient names for the estimated means
#' anov <- label_estimates(anov, c("site1", "site2", "site3","site4","site5"))
#' # set a seed value
#' set.seed(100)
#' # test hypotheses with bain
#' results <- bain(anov, "site1=site2=site3=site4=site5; site2>site5>site1>
#' site3>site4")
#' # display the results
#' print(results)
#' # obtain the descriptives table
#' descriptives(results, ci = 0.95)
#' #
#' #
#' # ===========================================================================
#' # USING BAIN WITH A LM OBJECT: EXAMPLE II
#' # An example of the Bayesian ANCOVA
#' # ===========================================================================
#' # load the bain package which includes the simulated sesamesim.txt data se
#' library(bain)
#' # make a factor of variable site
#' sesamesim$site <- as.factor(sesamesim$site)
#' # Center the covariate. If centered the coef() command below displays the
#' # adjusted means.
#' # If not centered the intercepts are displayed.
#' sesamesim$prenumb <- sesamesim$prenumb - mean(sesamesim$prenumb)
#' # execute an analysis of covariance using lm() which, due to the -1, returns
#' # estimates of the adjusted means per group
#' ancov <- lm(postnumb~site+prenumb-1,sesamesim)
#' # take a look at the estimated adjusted means, the regression coefficient
#' # of the covariate and their names
#' coef(ancov)
#' # choose convenient names for the estimated means
#' ancov <- label_estimates(ancov, c("s1", "s2", "s3","s4","s5","pre"))
#' # set a seed value
#' set.seed(100)
#' # test hypotheses with bain
#' results <- bain(ancov, "s1=s2=s3=s4=s5; s2>s5>s1>s3>s4")
#' # display the results
#' print(results)
#' # obtain the descriptives table
#' descriptives(results, ci = 0.95)
#' #
#' #
#' # ===========================================================================
#' # USING BAIN WITH A LM OBJECT: EXAMPLE III
#' # An example of the Bayesian multiple regression
#' # ===========================================================================
#' # load the bain package which includes the simulated sesamesim.txt data set
#' library(bain)
#' # execute a multiple regression using lm()
#' regr <- lm(postnumb ~ age + peabody + prenumb,sesamesim)
#' # take a look at the estimated regression coefficients and their names
#' coef(regr)
#' # choose convenient names for the estimated means
#' regr <- label_estimates(regr, c("int", "age", "peab", "pre"))
#' # set a seed value
#' set.seed(100)
#' # test hypotheses with bain. Note that standardized = FALSE denotes that the
#' # hypotheses are in terms of unstandardized regression coefficients
#' results<-bain(regr, "age = 0 & peab=0 & pre=0 ; age > 0 & peab > 0 & pre > 0"
#' , standardize = FALSE)
#' # display the results
#' print(results)
#' # obtain the descriptives table
#' descriptives(results, ci = 0.95)
#' # Since it is only meaningful to compare regression coefficients if they are
#' # measured on the same scale, bain can also evaluate standardized regression
#' # coefficients (based on the seBeta function by Jeff Jones and Niels Waller):
#' # set a seed value
#' set.seed(100)
#' # test hypotheses with bain. Note that standardized = TRUE denotes that the
#' # hypotheses are in terms of standardized regression coefficients
#' results<-bain(regr, "age = peab = pre ; pre > age > peab",
#' standardize = TRUE)
#' # display the results
#' print(results)
#' # obtain the descriptives table
#' descriptives(results, ci = 0.95)
#' #
#' #
#' # ===========================================================================
#' # USING BAIN WITH A NAMED VECTOR: EXAMPLE I
#' # An example of the Bayesian ANOVA using a named vector as input for bain
#' # ===========================================================================
#' # load the bain package which includes the simulated sesamesim.txt data se
#' library(bain)
#' # make a factor of variable site
#' sesamesim$site <- as.factor(sesamesim$site)
#' # execute an analysis of variance using lm() which, due to the -1, returns
#' # estimates of the means per group
#' anov <- lm(postnumb~site-1,sesamesim)
#' # take a look at the estimated means and their names
#' coef(anov)
#' # collect the estimates means in a vector
#' estimate <- coef(anov)
#' # give names to the estimates in anov
#' names(estimate) <- c("site1", "site2", "site3","site4","site5")
#' # create a vector containing the sample sizes of each group
#' ngroup <- table(sesamesim$site)
#' # compute for each group the covariance matrix of the parameters
#' # of that group and collect these in a list
#' # for the ANOVA this is simply a list containing for each group the variance
#' # of the mean note that, the within group variance as estimated using lm is
#' # used to compute the variance of each of the means! See, Hoijtink, Gu, and
#' # Mulder (2018) for further elaborations.
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
#' # characterized by one mean, therefore group_parameters=0. Note that are no
#' # joint parameters, therefore, joint_parameters=0.
#' results <- bain(estimate,
#' "site1=site2=site3=site4=site5; site2>site5>site1>site3>site4",
#' n=ngroup,Sigma=covlist,group_parameters=1,joint_parameters = 0)
#' # display the results
#' print(results)
#' # obtain the descriptives table
#' descriptives(results, ci = 0.95)
#' #
#' #
#' # ===========================================================================
#' # USING BAIN WITH A NAMED VECTOR: EXAMPLE II
#' # An example of the Bayesian ANCOVA using a named vector as input for bain
#' # ===========================================================================
#' library(bain)
#' # make a factor of variable site
#' sesamesim$site <- as.factor(sesamesim$site)
#' # center the covariate. If centered the coef() command below displays the
#' # adjusted means. If not centered the intercepts are displayed.
#' sesamesim$prenumb <- sesamesim$prenumb - mean(sesamesim$prenumb)
#' # execute an analysis of covariance using lm() which, due to the -1, returns
#' # estimates of the adjusted means per group
#' ancov2 <- lm(postnumb~site+prenumb-1,sesamesim)
#' # take a look at the estimated adjusted means and their names
#' coef(ancov2)
#' # collect the estimates of the adjusted means and regression coefficient of
#' # the covariate in a vector
#' estimates <- coef(ancov2)
#' # assign labels to the estimates
#' names(estimates)<- c("v.1", "v.2", "v.3", "v.4","v.5", "pre")
#' # compute the sample size per group
#' ngroup <- table(sesamesim$site)
#' # compute for each group the covariance matrix of the parameters of that
#' # group and collect these in a list note that, the residual variance as
#' # estimated using lm is used to compute these covariance matrices
#' var <- (summary(ancov2)$sigma)**2
#' # below, for each group, the covariance matrix of the adjusted mean and
#' # covariate is computed
#' # see Hoijtink, Gu, and Mulder (2018) for further explanation and elaboration
#' cat1 <- subset(cbind(sesamesim$site,sesamesim$prenumb), sesamesim$site == 1)
#' cat1[,1] <- 1
#' cat1 <- as.matrix(cat1)
#' cov1 <- var * solve(t(cat1) %*% cat1)
#' #
#' cat2 <- subset(cbind(sesamesim$site,sesamesim$prenumb), sesamesim$site == 2)
#' cat2[,1] <- 1
#' cat2 <- as.matrix(cat2)
#' cov2 <- var * solve(t(cat2) %*% cat2)
#' #
#' cat3 <- subset(cbind(sesamesim$site,sesamesim$prenumb), sesamesim$site == 3)
#' cat3[,1] <- 1
#' cat3 <- as.matrix(cat3)
#' cov3 <- var * solve(t(cat3) %*% cat3)
#' #
#' cat4 <- subset(cbind(sesamesim$site,sesamesim$prenumb), sesamesim$site == 4)
#' cat4[,1] <- 1
#' cat4 <- as.matrix(cat4)
#' cov4 <- var * solve(t(cat4) %*% cat4)
#' #
#' cat5 <- subset(cbind(sesamesim$site,sesamesim$prenumb), sesamesim$site == 5)
#' cat5[,1] <- 1
#' cat5 <- as.matrix(cat5)
#' cov5 <- var * solve(t(cat5) %*% cat5)
#' #
#' covariances <- list(cov1, cov2, cov3, cov4,cov5)
#' # set a seed value
#' set.seed(100)
#' # test hypotheses with bain. Note that there are multiple groups
#' # characterized by one adjusted mean, therefore group_parameters=0. Note that
#' # there is one covariate, therefore, joint_parameters=1.
#' results2<-bain(estimates,"v.1=v.2=v.3=v.4=v.5;v.2 > v.5 > v.1 > v.3 >v.4;",
#' n=ngroup,Sigma=covariances,group_parameters=1,joint_parameters = 1)
#' # display the results
#' print(results2)
#' # obtain the descriptives table
#' descriptives(results2, ci = 0.95)
#' #
#' #
#' # ===========================================================================
#' # USING BAIN WITH A NAMED VECTOR: EXAMPLE III
#' # An example of the Bayesian one within factor repeated measures analysis
#' # ===========================================================================
#' # load the bain package which includes the simulated sesamesim.txt data set
#' library(bain)
#' # estimate the means of three repeated measures of number knowledge
#' # the 1 denotes that these means have to be estimated
#' within <- lm(cbind(prenumb,postnumb,funumb)~1, data=sesamesim)
#' # take a look at the estimated means and their names
#' coef(within)
#' # note that the model specified in lm has three dependent variables.
#' # Consequently, the estimates rendered by lm are collected in a "matrix".
#' # Since bain needs a named vector containing the estimated means, the [1:3]
#' # code is used to select the three means from a matrix and store them in a
#' # vector.
#' estimate <- coef(within)[1:3]
#' # give names to the estimates in anov
#' names(estimate) <- c("pre", "post", "fu")
#' # compute the sample size
#' ngroup <- nrow(sesamesim)
#' # compute the covariance matrix of the three means
#' covmatr <- list(vcov(within))
#' # set a seed value
#' set.seed(100)
#' # test hypotheses with bain. Note that there is one group, therefore
#' # group_parameters=0.
#' # Note that there are three means, therefore, joint_parameters=3.
#' results <- bain(estimate,"pre = post = fu; pre < post < fu", n=ngroup,
#' Sigma=covmatr, group_parameters=3, joint_parameters = 0)
#' # display the results
#' print(results)
#' # obtain the descriptives table
#' descriptives(results, ci = 0.95)
#' #
#' #
#' # ===========================================================================
#' # USING BAIN WITH A NAMED VECTOR: EXAMPLE IV
#' # An example of the Bayesian one between factor and one within factor
#' # repeated measures analysis
#' # ===========================================================================
#' # load the bain package which includes the simulated sesamesim.txt data set
#' library(bain)
#' # make a factor of the variable sex
#' sesamesim$sex <- factor(sesamesim$sex)
#' # estimate the means of prenumb, postnumb, and funumb for boys and girls
#' # the -1 denotes that the means have to estimated
#' bw <- lm(cbind(prenumb, postnumb, funumb)~sex-1, data=sesamesim)
#' # take a look at the estimated means and their names
#' coef(bw)
#' # collect the estimated means in a vector
#' est1 <-coef(bw)[1,1:3] # the three means for sex = 1
#' est2 <-coef(bw)[2,1:3] # the three means for sex = 2
#' estimate <- c(est1,est2)
#' # give names to the estimates in anov
#' names(estimate) <- c("pre1", "post1", "fu1","pre2", "post2", "fu2")
#' # determine the sample size per group
#' ngroup<-table(sesamesim$sex)
#' # cov1 has to contain the covariance matrix of the three means in group 1.
#' # cov2 has to contain the covariance matrix in group 2
#' # typing vcov(bw) in the console pane highlights the structure of
#' # the covariance matrix of all 3+3=6 means
#' # it has to be dissected in to cov1 and cov2
#' cov1 <- c(vcov(bw)[1,1],vcov(bw)[1,3],vcov(bw)[1,5],vcov(bw)[3,1],
#' vcov(bw)[3,3],vcov(bw)[3,5],vcov(bw)[5,1],vcov(bw)[5,3],vcov(bw)[5,5])
#' cov1 <- matrix(cov1,3,3)
#' cov2 <- c(vcov(bw)[2,2],vcov(bw)[2,4],vcov(bw)[2,6],vcov(bw)[4,2],
#' vcov(bw)[4,4],vcov(bw)[4,6],vcov(bw)[6,2],vcov(bw)[6,4],vcov(bw)[6,6])
#' cov2 <- matrix(cov2,3,3)
#' covariance<-list(cov1,cov2)
#' # set a seed value
#' set.seed(100)
#' # test hypotheses with bain. Note that there are multiple groups
#' # characterized by three means, therefore group_parameters=3. Note that there
#' # are no additional parameters, therefore, joint_parameters=0.
#' results <-bain(estimate, "pre1 - pre2 = post1 - post2 = fu1 -fu2;
#' pre1 - pre2 > post1 - post2 > fu1 -fu2"  , n=ngroup, Sigma=covariance,
#' group_parameters=3, joint_parameters = 0)
#' # display the results
#' print(results)
#' # obtain the descriptives table
#' descriptives(results, ci = 0.95)
#' #
#' #
#' # ===========================================================================
#' # USING BAIN WITH A NAMED VECTOR: EXAMPLE V
#' # An example of the Bayesian logistic regression as the counterpart of
#' # Bayesian multiple regression
#' # ===========================================================================
#' # load the bain package which includes the simulated sesamesim.txt data set
#' library(bain)
#' # regression coefficients can only be mutually compared if the
#' # corresponding predictors are measured on the same scale, therefore the
#' # predictors are standardized
#' sesamesim$age <- (sesamesim$age - mean(sesamesim$age))/sd(sesamesim$age)
#' sesamesim$peabody <- (sesamesim$peabody - mean(sesamesim$peabody))/
#' sd(sesamesim$peabody)
#' sesamesim$prenumb <- (sesamesim$prenumb - mean(sesamesim$prenumb))/
#' sd(sesamesim$prenumb)
#' # estimate the logistic regression coefficients
#' logreg <- glm(viewenc ~ age + peabody + prenumb, family=binomial,
#' data=sesamesim)
#' # take a look at the estimates and their names
#' coef(logreg)
#' # collect the estimated intercept and regression coefficients in a vector
#' estimate <- coef(logreg)
#' # give names to the estimates
#' names(estimate) <- c("int", "age", "peab" ,"pre" )
#' # compute the sample size. NOte that, this is an analysis with ONE group
#' ngroup <- nrow(sesamesim)
#' # compute the covariance matrix of the intercept and regression coefficients
#' covmatr <- list(vcov(logreg))
#' # set a seed value
#' set.seed(100)
#' # test hypotheses with bain. Note that there is one group, therefore
#' # group_parameters=0.
#' # Note that there are one intercept and three regression coefficients,
#' therefore, joint_parameters=4.
#' results <- bain(estimate, "age = peab = pre; age > pre > peab", n=ngroup,
#' Sigma=covmatr, group_parameters=4, joint_parameters = 0)
#' # display the results
#' print(results)
#' # obtain the descriptives table
#' descriptives(results, ci = 0.95)
#'
#' # ===========================================================================
#' # USING BAIN WITH A NAMED VECTOR: EXAMPLE VI
#' # An example of the Bayesian logistic regression as the counterpart of
#' # an ANCOVA. This is Example 3 from  Hoijtin, Gu, and Mulder (2018).
#' # The research question concerns the probability of encouragement to view
#' # sesame street (a dichotomous 0/1 meaning no/yes dependent variable) for
#' # boys (sex=1) and girls (sex=2) adjusted for their age. This is a kind of
#' # ANCOVA with a dichotomous instead of a continuous dependent variable.
#' # ===========================================================================
#' # load the bain package which includes the simulated sesamesim.txt data set
#' library(bain)
#' # load the numDeriv package which will be used to compute the covariance
#' # matrix
#' # of the adjusted group coefficients and regression coefficient of age
#' # for the boys and the girls
#' # using the estimates obtained using the data for the boys AND the girls.
#' library(numDeriv)
#' # make a factor of the variable sex
#' sesamesim$sex <- factor(sesamesim$sex)
#' # center the covariate age
#' sesamesim$age <- sesamesim$age - mean(sesamesim$age)
#' # determine sample size per sex group
#' ngroup <- table(sesamesim$sex)
#' # execute the logistic regression, -1 ensures that the coefficients
#' # for boys and girl are estimated adjusted for the covariate age
#' anal <- glm(viewenc ~ sex + age - 1, family=binomial, data=sesamesim)
#' # take a look at the estimates and their names
#' coef(logreg)
#' # collect the estimates obtained using the data of the boys AND the
#' # girls in a vector
#' estimates <-coef(anal)
#' # give names to the estimates
#' names(estimates) <- c("boys", "girls", "age")
#' # use numDeriv to compute the Hessian matrix and subsequently the
#' # covariance matrix for
#' # each of the two (boys and girls) groups
#' # The vector f should contain the regression coefficient of the group
#' # at hand and the
#' # regression coefficientof the covariate
#' #
#' # the first group
#' data <- subset(cbind(sesamesim$sex,sesamesim$age,sesamesim$viewenc),
#' sesamesim$sex==1)
#' f <- 1
#' f[1] <- estimates[1] # the regression coefficient of boys
#' f[2] <- estimates[3] # the regression coefficient of age
#' #
#' # within the for loop below the log likelihood of the logistic
#' # regression model is computed
#' # using the data for the boys
#' logist1 <- function(x){
#'   out <- 0
#'   for (i in 1:ngroup[1]){
#'     out <- out + data[i,3]*(x[1] + x[2]*data[i,2]) - log (1 +
#'     exp(x[1] + x[2]*data[i,2]))
#'   }
#'   return(out)
#' }
#' hes1 <- hessian(func=logist1, x=f)
#' # multiply with -1 and invert to obtain the covariance matrix for the
#' # first group
#' cov1 <- -1 * solve(hes1)
#' #
#' # the second group
#' data <- subset(cbind(sesamesim$sex,sesamesim$age,sesamesim$viewenc),
#' sesamesim$sex==2)
#' f[1] <- estimates[2] # the regression coefficient of girls
#' f[2] <- estimates[3] # the regression coefficient of age
#'
#' # within the for loop below the log likelihood of the logistic
#' # regression model is computed
#' # using the data for the girls
#' logist2 <- function(x){
#'   out <- 0
#'   for (i in 1:ngroup[2]){ # samp[2] is the sample size of the second
#'   group
#'     out <- out + data[i,3]*(x[1] + x[2]*data[i,2]) - log (1 +
#'     exp(x[1] + x[2]*data[i,2]))
#'   }
#'   return(out)
#' }
#' hes2 <- hessian(func=logist2, x=f)
#' # multiply with -1 and invert to obtain the covariance matrix
#' cov2 <- -1 * solve(hes2)
#' #
#' #make a list of covariance matrices
#' covariance<-list(cov1,cov2)
#' #
#' # set a seed value
#' set.seed(100)
#' # test hypotheses with bain. Note that there are multiple groups
#' # characterized by one adjusted group coefficient,
#' # therefore group_parameters=1. Note that there is one covariate,
#' # therefore, joint_parameters=1.
#' results <- bain(estimates, "boys < girls & age > 0", n=ngroup,
#' Sigma=covariance, group_parameters=1, joint_parameters = 1)
#' # display the results
#' print(results)
#' # obtain the descriptives table
#' descriptives(results, ci = 0.95)
#' #
#' #
#' # ===========================================================================
#' # USING BAIN WITH A NAMED VECTOR: EXAMPLE VII
#' # An example of the Bayesian robust ANOVA with unequal within group
#' # variances. Robust
#' # ANOVA is elaborated in Bosman and Hoijtink (unpublished).
#' # ===========================================================================
#' # load the bain package which includes the simulated sesamesim.txt data set
#' library(bain)
#' # load the WRS2 package which renders the trimmed sample mean and
#' # corresponding standard error
#' library(WRS2)
#' # make a factor of variable site
#' sesamesim$site <- as.factor(sesamesim$site)
#' # create a vector containing the sample sizes of each group
#' ngroup <- table(sesamesim$site)
#' # Compute the 20% sample trimmed mean for each site
#' estimates <- c(mean(sesamesim$postnumb[sesamesim$site == 1], tr = 0.2),
#'                mean(sesamesim$postnumb[sesamesim$site == 2], tr = 0.2),
#'                mean(sesamesim$postnumb[sesamesim$site == 3], tr = 0.2),
#'                mean(sesamesim$postnumb[sesamesim$site == 4], tr = 0.2),
#'                mean(sesamesim$postnumb[sesamesim$site == 5], tr = 0.2))
#' # give names to the estimates
#' names(estimates) <- c("s1", "s2", "s3","s4","s5")
#' # display the estimates and their names
#' print(estimates)
#' # Compute the sample trimmed mean standard error for each site
#' se <- c(trimse(sesamesim$postnumb[sesamesim$site == 1]),
#'         trimse(sesamesim$postnumb[sesamesim$site == 2]),
#'         trimse(sesamesim$postnumb[sesamesim$site == 3]),
#'         trimse(sesamesim$postnumb[sesamesim$site == 4]),
#'         trimse(sesamesim$postnumb[sesamesim$site == 5]))
#' # Square the standard errors to obtain the variances of the sample
#' # trimmed means
#' var <- se^2
#' # Store the variances in a list of matrices
#' covlist <- list(matrix(var[1]),matrix(var[2]),
#' matrix(var[3]),matrix(var[4]), matrix(var[5]))
#' # set a seed value
#' set.seed(100)
#' # test hypotheses with bain
#' results <- bain(estimates,"s1=s2=s3=s4=s5;
#' s2>s5>s1>s3>s4",n=ngroup,Sigma=covlist,group_parameters=1,joint_parameters
#' = 0)
#' # display the results
#' print(results)
#' # obtain the descriptives table
#' descriptives(results, ci = 0.95)
#' #
#' #
#' # ===========================================================================
#' # USING BAIN WITH A NAMED VECTOR: EXAMPLE VIII
#' # An example of the Bayesian Regression with missing data. The computation
#' # of Bayes factors in the presence of missing data is elaborated in
#' # Hoijtink, Gu, Mulder, and Rosseel (in press).
#' #
#' # RUNNING THIS EXAMPLE WILL TAKE ABOUT 60 SECONDS
#' # ===========================================================================
#' # load the bain package which includes the simulated sesamesim.txt data se
#' library(bain
#' # load the mice (multiple imputation of missing data), psyc
#' # (provides access to the describe function), and MASS libraries
#' # inspect the help file to obtain further information abou
#' # mice - also surf to http://www.stefvanbuuren.nl/mi/MICE.html t
#' # obtain further information an
#' # references for mice
#' library(mice
#' library(psych
#' library(MASS
#' sesamesim <
#' cbind(sesamesim$prenumb,sesamesim$postnumb,sesamesim$funumb
#' sesamesim$peabody
#' colnames(sesamesim) <- c("prenumb","postnumb","funumb","peabody"
#' sesamesim <- as.data.frame(sesamesim
#' # this examples is based on the prenumb, postnumb, funumb and peabod
#' # variables in the sesamesim data set. First of all, missing data ar
#' # created in these four variables
#'
#' set.seed(1
#' pmis1<-
#' pmis2<-
#' pmis3<-
#'
#' for (i in 1:240)
#'   pmis1[i] <
#'   exp(-30+sesamesim$prenumb[i]+sesamesim$postnumb[i])/(1+exp
#'   -30+sesamesim$prenumb[i]+sesamesim$postnumb[i])
#'   pmis2[i]<- exp(-30+sesamesim$peabody[i])/(1+exp(-30+sesamesim$peabody[i])
#'   pmis3[i]<- .9
#'
#'   uni<-runif(1
#'   if (pmis1[i] < uni)
#'     sesamesim$funumb[i]<-Na
#'
#'   uni<-runif(1
#'   if (pmis2[i] < uni)
#'     sesamesim$prenumb[i]<-Na
#'     sesamesim$postnumb[i]<-Na
#'
#'   uni<-runif(1
#'   if (pmis3[i] < uni)
#'     sesamesim$peabody[i]<-Na
#'
#'
#' # print data summaries - note that due to missing valus the n pe
#' # variable is smaller than 24
#' print(describe(sesamesim)
#' # use mice to create 1000 imputed data matrices. Note that, th
#' # approach used below is only one manner in whic
#' # mice can be instructed. Many other options are available
#' M <- 100
#' out <- mice(data = sesamesim, m = M, seed=999
#' meth=c("norm","norm","norm","norm"), diagnostics = FALSE, printFlag
#' FALSE
#' # create matrices in which 1000 vectors with estimates can be store
#' # and in which a covariance matrix can be store
#' mulest <- matrix(0,nrow=1000,ncol=2
#' covwithin <- matrix(0,nrow=2,ncol=2
#' # execute 1000 multiple regressions using the imputed data matrices an
#' # store the estimate
#' # of only the regression coefficients of funumb on prenumb an
#' # postnumband and the average of the 1000 covariance matrices
#' # See Hoijtink, Gu, Mulder, and Rosseel (in press) for an explanatio
#' # of the latter
#' for(i in 1:M)
#' mulres <- lm(funumb~prenumb+postnumb,complete(out,i)
#' mulest[i,]<-coef(mulres)[2:3
#' covwithin<-covwithin + 1/M * vcov(mulres)[2:3,2:3
#'
#' # Compute the average of the estimates and assign names, the betwee
#' # and total covariance matrix
#' # See Hoijtink, Gu, Mulder, and Rosseel (in press) for an explanation
#' estimates <- colMeans(mulest
#' names(estimates) <- c("prenumb", "postnumb"
#' covbetween <- cov(mulest
#' covariance <- covwithin + (1+1/M)*covbetwee
#' # determine the sample siz
#' samp <- nrow(sesamesim
#' # compute the effective sample siz
#' # See Hoijtink, Gu, Mulder, and Rosseel (in press) for an explanation
#' nucom<-samp-length(estimates
#' lam <- (1+1/M)*(1/length(estimates))* tr(covbetween %*% ginv(covariance)
#' nuold<-(M-1)/(lam^2
#' nuobs<-(nucom+1)/(nucom+3)*nucom*(1-lam
#' nu<- nuold*nuobs/(nuold+nuobs
#' fracmis <- (nu+1)/(nu+3)*lam + 2/(nu+3)
#' neff<-samp-samp*fracmis
#' covariance <- list(covariance)
#' # set the seed
#' set.seed(100)
#' # test hypotheses with bain
#' results <-
#' bain(estimates,"prenumb=postnumb=0",n=neff,Sigma=covariance,group_parameters=
#' 2,joint_parameters
#' = 0)
#' # display the results
#' print(results)
#' # obtain the descriptives table
#' descriptives(results, ci = 0.95)
#' #
#'
#' # ===========================================================================
#' # USING BAIN WITH A NAMED VECTOR: EXAMPLE IX
#' # An example of the Bayesian structural equation modelling. For this
#' # topic additional
#' # examples are elaborated in Gu, Hoijtink, Mulder, and Rosseel (unpublished).
#' # ===========================================================================
#' # load the bain package
#' library(bain)
#' # load the lavaan package - look at the help file to obtain further
#' # information about lavaan - also surf to
#' # lavaan.ugent.be to obtain further information and references for
#' # lavaan - this example uses the
#' # HolzingerSwineford1939 data set which is included with lavaan.
#' library(lavaan)
#' # Specify a latent regression model
#' model <- 'visual  =~ x1 + x2 + x3
#' textual =~ x4 + x5 + x6
#' speed   =~ x7 + x8 + x9
#' speed ~ textual + visual'
#' # Estimate the parameters of the latent regression model with lavaan
#' fit<-sem(model,data=HolzingerSwineford1939,std.lv = TRUE)
#' # determine the sample size
#' ngroup<-nobs(fit)
#' # collect the "standardized" estimates of the latent regression
#' # coefficients in a vector
#' # typing standardizedSolution(fit) in the console pane shows that the
#' # estimates can be
#' # found in the fourth column of rows 10 and 11.
#' estimate<-standardizedSolution(fit)[10:11,4]
#' # assign names to the estimates
#' names(estimate) <- c("textual","visual")
#' # determine the covariance matrix of the estimates
#' # typing lavInspect(fit, "vcov.std.all") in the console pane shows that
#' # the estimates can be
#' # found in the rows 10 and 11 crossed with columns 10 and 11
#' covariance<-list(lavInspect(fit, "vcov.std.all")[10:11,10:11])
#' # set a seed value
#' set.seed(100)
#' # test hypotheses with bain
#' results <- bain(estimate,"visual=textual=0; visual > textual >
#' 0",n=ngroup,Sigma=covariance,group_parameters=2,joint_parameters = 0)
#' # display the results
#' print(results)
#' # obtain the descriptives table
#' descriptives(results, ci = 0.95)
#' }
#'
#'
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
             names(Args$x) <- gsub(paste0("^", names(df)[var_factor]), "", names(Args$x))
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
