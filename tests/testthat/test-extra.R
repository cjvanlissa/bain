# ==============================================================================
# TEST NUMBER 2: PARAMETER FIXED AT A VALUE
# ==============================================================================

# read in the simulated sesamestreet data
sesamedata <- sesamesim

# use lavaan syntax to specify the confirmatory factor model
model1 <- 'Ab ~ .5 * Bb + Bl + 1'

# use the lavaan sem function to execute the confirmatory factor analysis
fit1 <- sem(model1, data = sesamedata)

# HERE FOLLOWS THE CALL TO THE BAIN S3 FUNCTION WITH UNSTANDARDIZED PARAMETERS

hypotheses1 <-" Ab~Bl = 0; Ab~Bl > 0"
set.seed(100)
y2 <- bain(fit1,hypotheses1,standardized = TRUE)


# HERE FOLLOWS THE CALL TO BAIN DEFAULT WITH STANDARDIZED PARAMETERS
PE1 <- parameterEstimates(fit1, standardized = TRUE)
estimate1 <- PE1[ PE1$op == "~", "std.all"][2]
names(estimate1) <- c("b")
cov<- matrix(lavInspect(fit1, "vcov.std.all")[1, 1],1,1)
covariance1 <- list(cov)
ngroup1 <- nobs(fit1)
hypotheses1 <-"b=0; b>0"
z2 <- bain(estimate1, hypotheses1, n =ngroup1, Sigma = covariance1,
           group_parameters = 1, joint_parameters = 0)

# HERE FOLLOWS THE CHECK IF S3 GIVES THE SAME RESULTS AS DEFAULT

# TEST RESULTS

test_that("Bain mutual", {expect_equal(y2$fit$Fit , z2$fit$Fit)})
test_that("Bain mutual", {expect_equal(y2$fit$Com , z2$fit$Com)})
test_that("Bain mutual", {expect_equal(y2$independent_restrictions, z2$independent_restrictions)})
test_that("Bain mutual", {expect_equal(y2$b, z2$b)})
test_that("Bain mutual", {expect_equal(as.vector(y2$posterior[1,1]), as.vector(z2$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(y2$prior[1,1]), as.vector(z2$prior))})
test_that("Bain mutual", {expect_equal(y2$fit$BF,z2$fit$BF)})
test_that("Bain mutual", {expect_equal(y2$fit$PMPb , z2$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(y2$BFmatrix)), as.vector(t(z2$BFmatrix)))})

# ==============================================================================
# TEST NUMBER 3A: ABBREVIATIONS OF THE NAMES - SINGLE GROUP SHORT NAMES
# ==============================================================================

# read in the simulated sesamestreet data
sesamedata <- sesamesim

# use lavaan syntax to specify the confirmatory factor model
model1 <- 'age ~ peabody + sex + 1'

# use the lavaan sem function to execute the confirmatory factor analysis
fit1 <- sem(model1, data = sesamedata)

hypotheses1 <-"age~pe > age~s"
set.seed(100)
y1 <- bain(fit1,hypotheses1,standardized = TRUE)

# HERE FOLLOWS THE CALL TO BAIN DEFAULT WITH UNSTANDARDIZED PARAMETERS
PE1 <- parameterEstimates(fit1, standardized = TRUE)
estimate1 <- PE1[ PE1$op == "~", "std.all"][1:2]
names(estimate1) <- c("a","b")
cov<- matrix(lavInspect(fit1, "vcov.std.all")[1:2, 1:2],2,2)
covariance1 <- list(cov)
ngroup1 <- nobs(fit1)
hypotheses1 <-" a > b"
set.seed(100)
z1 <- bain(estimate1, hypotheses1, n =ngroup1, Sigma = covariance1,
           group_parameters = 2, joint_parameters = 0)

# HERE FOLLOWS THE CHECK IF S3 GIVES THE SAME RESULTS AS DEFAULT

# TEST RESULTS UNSTANDARDIZED

test_that("Bain mutual", {expect_equal(y1$fit$Fit , z1$fit$Fit)})
test_that("Bain mutual", {expect_equal(y1$fit$Com , z1$fit$Com)})
test_that("Bain mutual", {expect_equal(y1$independent_restrictions, z1$independent_restrictions)})
test_that("Bain mutual", {expect_equal(y1$b, z1$b)})
test_that("Bain mutual", {expect_equal(as.vector(y1$posterior[1:2,1:2]), as.vector(z1$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(y1$prior[1:2,1:2]), as.vector(z1$prior))})
test_that("Bain mutual", {expect_equal(y1$fit$BF,z1$fit$BF)})
test_that("Bain mutual", {expect_equal(y1$fit$PMPb , z1$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(y1$BFmatrix)), as.vector(t(z1$BFmatrix)))})

# ==============================================================================
# TEST NUMBER 3B: ABBREVIATIONS OF THE NAMES - MULTIPLE GROUP SHORT NAMES
# ==============================================================================

model1 <- 'age ~ peabody + 1'

sesamesim$sex <- factor(sesamesim$sex)
fit1 <- sem(model1, data = sesamesim, group = "sex")
hypotheses1 <-"age~peabody.1 = age~peabody.2"
set.seed(100)
y1 <- bain(fit1,hypotheses1,standardized = TRUE)

sesamesim$sex <- factor(sesamesim$sex, labels = c("boy", "girl"))
fit1 <- sem(model1, data = sesamesim, group = "sex")
hypotheses1 <-"age~peabody.boy = age~peabody.girl"
set.seed(100)
y2 <- bain(fit1,hypotheses1,standardized = TRUE)

sesamesim$sex <- factor(sesamesim$sex, labels = c("boy", "girl"))
fit1 <- sem(model1, data = sesamesim, group = "sex")
hypotheses1 <-"age~peabody.b = age~peabody.gi"
set.seed(100)
y3 <- bain(fit1,hypotheses1,standardized = TRUE)

test_that("Bain mutual", {expect_equal(y1$fit$BF,y2$fit$BF)})
test_that("Bain mutual", {expect_equal(y2$fit$BF,y3$fit$BF)})


# ==============================================================================
# TEST NUMBER 4: EXAMPLE 2 ZONDER STDLV = TRUE
# ==============================================================================


sesamedata <- sesamesim

model2 <- '
A  =~ Ab + Al + Af + An + Ar + Ac
B =~ Bb + Bl + Bf + Bn + Br + Bc

A ~ B + age + peabody
'
fit2 <- sem(model2, data = sesamedata, std.lv = FALSE)

# HERE FOLLOWS THE CALL TO THE BAIN S3 FUNCTION:

hypotheses2 <- "A~B > A~peabody = A~age = 0;
A~B > A~peabody > A~age = 0;
A~B > A~peabody > A~age > 0"

set.seed(100)
y1 <- bain(fit2, hypotheses2, scalefactor = 1, standardized = TRUE)

# HERE FOLLOWS THE CALL TO BAIN DEFAULT

ngroup2 <- nobs(fit2)

PE2 <- parameterEstimates(fit2, standardized = TRUE)
# here, we only need the rows that correspond to regressions (ie op == "~"):
estimate2 <- PE2[ PE2$op == "~", "std.all"]

names(estimate2) <- c("before", "age", "pea")

PT2 <- parTable(fit2)
par.idx2 <- PT2$free[ PT2$op == "~" ]

covariance2 <- list(lavInspect(fit2, "vcov.std.all")[par.idx2, par.idx2])

hypotheses2 <- "before > pea = age = 0;
before > pea > age = 0;
before > pea > age > 0"

set.seed(100)
z1 <- bain(estimate2, hypotheses2, n = ngroup2, Sigma = covariance2,
           group_parameters = 3,joint_parameters = 0)

# HERE FOLLOWS THE CHECK IF S3 GIVES THE SAME RESULTS AS DEFAULT

test_that("Bain mutual", {expect_equal(y1$fit$Fit , z1$fit$Fit)})
test_that("Bain mutual", {expect_equal(y1$fit$Com , z1$fit$Com)})
test_that("Bain mutual", {expect_equal(y1$independent_restrictions, z1$independent_restrictions)})
test_that("Bain mutual", {expect_equal(y1$b, z1$b)})
test_that("Bain mutual", {expect_equal(as.vector(y1$posterior[11:13,11:13]), as.vector(z1$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(y1$prior[11:13,11:13]), as.vector(z1$prior))})
test_that("Bain mutual", {expect_equal(y1$fit$BF,z1$fit$BF)})
test_that("Bain mutual", {expect_equal(y1$fit$PMPb , z1$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(y1$BFmatrix)), as.vector(t(z1$BFmatrix)))})



# ==============================================================================
# TEST NUMBER 7: A MULTIPLE GROUP MODEL WITH BETWEEN CONSTRAINTS
# ==============================================================================


model1 <- 'age ~ peabody + 1'

# AS IT SHOULD BE THIS ANALYSIS DOES NOT WORK. HOW TO TRANSLATE IT
# INTO A TESTTHAT STATEMENT?

sesamesim$sex <- factor(sesamesim$sex)
fit1 <- sem(model1, data = sesamesim, group = "sex",group.equal = c("intercepts"))
hypotheses1 <-"age~peabody.1 = age~peabody.2"
set.seed(100)
test_that("Multiple group model throws error", expect_error(y1 <- bain(fit1,hypotheses1,standardized = TRUE)))

# ==============================================================================
# TEST NUMBER 8: TEST THAT DEFINED PARAMETERS ARE DROPPED
# ==============================================================================


# BELOW THE CALCULATED PAR IS CORRECTLY NOT RECOGNIZED
# HOW TO TRANSLATE THAT IN A TESTTHAT STATEMENT?

sesamedata <- sesamesim
model1 <- 'age ~ a*peabody + b*sex + 1
           def := a*b'
fit1 <- sem(model1, data = sesamedata)

set.seed(100)
test_that("Defined parameters are excluded", expect_error(y1 <- bain(fit1, "def = .4", standardized = TRUE)))

# ==============================================================================
# TEST NUMBER 9: TEST THAT MULTILEVEL MODELS DO NOT WORK
# ==============================================================================

# THIS ONE GIVES THE CORRECT ERROR MESSAGE. HOW TO TRANSLATE
# THAT INTO A TESTTHAT STATEMENT?

model <- '
level: 1
fw =~ y1 + y2 + y3
fw ~ x1 + x2 + x3
level: 2
fb =~ y1 + y2 + y3
fb ~ w1 + w2
'
fit1 <- sem(model, data = Demo.twolevel, cluster = "cluster")
hypotheses1 <-"fw~x1=0"
set.seed(100)

test_that("Multilevel models return error", expect_error(y1 <- bain(fit1,hypotheses1,standardized = TRUE)))



