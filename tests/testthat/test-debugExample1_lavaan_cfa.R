data(sesamesim)
sesameCFA <- sesamesim
names(sesameCFA)[6] <- "pea"
model1 <- '
A =~ Ab + Al + Af + An + Ar + Ac
B =~ Bb + Bl + Bf + Bn + Br + Bc
'

# use the lavaan sem function to execute the confirmatory factor analysis
fit1 <- sem(model1, data = sesameCFA, std.lv = TRUE)

# HERE FOLLOWS THE CALL TO THE BAIN S3 FUNCTION:

hypotheses1 <-
  " A=~Ab > .6 & A=~Al > .6 & A=~Af > .6 & A=~An > .6 & A=~Ar > .6 & A=~Ac >.6 &
B=~Bb > .6 & B=~Bl > .6 & B=~Bf > .6 & B=~Bn > .6 & B=~Br > .6 & B=~Bc >.6"

set.seed(100)

y <- bain(fit1,hypotheses1,standardize = TRUE)
sy <- summary(y, ci = 0.95)

# HERE FOLLOWS THE CALL TO BAIN DEFAULT

# obtain the required input for bain:

# the sample size
ngroup1 <- lavaan::nobs(fit1)

# the parameterEstimates() function presents the estimated parameters
# in a data.frame; the additional argument standardize = TRUE add
# extra columns with standardize versions of these parameter estimates;
# for our purposes, we need the 'std.all' column, which means that both
# the observed and the latent variables have been standardize.
#
# we capture the output of parameterEstimates() in an object 'PE'
PE1 <- lavaan::parameterEstimates(fit1, standardize = TRUE)

# we only need the rows that correspond to factor loadings (ie op == "=~")
# and the column "std.all":
estimate1 <- PE1[ PE1$op == "=~", "std.all"]

# assign names to the estimates of the standardize factor loadingts
names(estimate1) <- c("Ab", "Al", "Af", "An", "Ar", "Ac",
                     "Bb", "Bl", "Bf", "Bn", "Br", "Bc")

# we will compute the full covariance matrix of standardize parameter
# estimates, and only extract the part we need: the rows/cols that
# correspond to the factor loadings
#
# to find out which rows/cols we need, we can look at the full parameter
# table:
PT1 <- parTable(fit1)
# and extract the parameter numbers (in the "free" column) that correspond
# to the factor loadings:
par.idx1 <- PT1$free[ PT1$op == "=~" ]

# obtain the covariance matrix of the standardize factor loadings
covariance1 <- list(lavInspect(fit1, "vcov.std.all")[par.idx1, par.idx1])

# specify the hypotheses using the syntax implementen in bain. Note that
# the & is used to combine different parts into one hypothesis. Note
# furthermore, that the ; is used to separate hypotheses
hypotheses1 <-
   " Ab > .6 & Al > .6 & Af > .6 & An > .6 & Ar > .6 & Ac >.6 &
     Bb > .6 & Bl > .6 & Bf > .6 & Bn > .6 & Br > .6 & Bc >.6"

# evaluate the hypotheses using bain, the command used has been explained in
# the paper. Don't forget to set the seed first in order to obtain results
# that can be reproduced exactly.
set.seed(100)
z <- bain(estimate1, hypotheses1, n =ngroup1, Sigma = covariance1,
                group_parameters = 12, joint_parameters = 0)

# print the results of the bain analysis and obtain the estimates and 95%
# central credibility intervals
sz <- summary(z, ci = 0.95)

# HERE FOLLOWS THE CHECK IF S3 GIVES THE SAME RESULTS AS DEFAULT

# TEST RESULTS

test_that("Bain mutual", {expect_equal(y$fit$Fit , z$fit$Fit)})
test_that("Bain mutual", {expect_equal(y$fit$Com , z$fit$Com)})
test_that("Bain mutual", {expect_equal(y$independent_restrictions, z$independent_restrictions)})
test_that("Bain mutual", {expect_equal(y$b, z$b)})
test_that("Bain mutual", {expect_equal(as.vector(y$posterior), as.vector(z$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(y$prior), as.vector(z$prior))})
test_that("Bain mutual", {expect_equal(y$fit$BF,z$fit$BF)})
test_that("Bain mutual", {expect_equal(y$fit$PMPb , z$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(y$BFmatrix)), as.vector(t(z$BFmatrix)))})

# TEST SUMMARY

test_that("summary", {expect_equal(sy$Estimate , sz$Estimate)})
test_that("summary", {expect_equal(sy$n , sz$n)})
test_that("summary", {expect_equal(sy$lb , sz$lb)})
test_that("summary", {expect_equal(sy$ub , sz$ub)})
