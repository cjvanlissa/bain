data(sesamesim)
sesameCFA <- sesamesim
names(sesameCFA)[6] <- "pea"
df <- sesameCFA
model3 <- '
    A  =~ Ab + Al + Af + An + Ar + Ac
    B =~ Bb + Bl + Bf + Bn + Br + Bc

    A ~ B + age + pea
'

df$sex <- factor(df$sex, labels = c("boy", "girl"))
# fit a multiple group latent regression model
fit3 <- lavaan::sem(model3, data = df, std.lv = TRUE, group = "sex")

# HERE FOLLOWS THE CALL TO THE BAIN S3 FUNCTION:

hypotheses31 <-
  "A=~Ab.boy = A=~Ab.girl &
A=~Al.boy = A=~Al.girl &
A=~Af.boy = A=~Af.girl &
A=~An.boy = A=~An.girl &
A=~Ar.boy = A=~Ar.girl &
A=~Ac.boy = A=~Ac.girl &
B=~Bb.boy = B=~Bb.girl &
B=~Bl.boy = B=~Bl.girl &
B=~Bf.boy = B=~Bf.girl &
B=~Bn.boy = B=~Bn.girl &
B=~Br.boy = B=~Br.girl &
B=~Bc.boy = B=~Bc.girl &
Ab~1.boy = Ab~1.girl &
Al~1.boy = Al~1.girl &
Af~1.boy = Af~1.girl &
An~1.boy = An~1.girl &
Ar~1.boy = Ar~1.girl &
Ac~1.boy = Ac~1.girl &
Bb~1.boy = Bb~1.girl &
Bl~1.boy = Bl~1.girl &
Bf~1.boy = Bf~1.girl &
Bn~1.boy = Bn~1.girl &
Br~1.boy = Br~1.girl &
Bc~1.boy = Bc~1.girl
"
set.seed(100)
y1 <- bain(fit3, hypotheses31, standardized = TRUE)

sy1 <- summary(y1, ci = 0.90)

hypotheses32 <-
  "A=~Ab.boy = A=~Ab.girl &
A=~Al.boy = A=~Al.girl &
A=~Af.boy = A=~Af.girl &
A=~An.boy = A=~An.girl &
A=~Ar.boy = A=~Ar.girl &
A=~Ac.boy = A=~Ac.girl &
B=~Bb.boy = B=~Bb.girl &
B=~Bl.boy = B=~Bl.girl &
B=~Bf.boy = B=~Bf.girl &
B=~Bn.boy = B=~Bn.girl &
B=~Br.boy = B=~Br.girl &
B=~Bc.boy = B=~Bc.girl &
Ab~1.boy = Ab~1.girl &
Al~1.boy = Al~1.girl &
Af~1.boy = Af~1.girl &
An~1.boy = An~1.girl &
Ar~1.boy = Ar~1.girl &
Ac~1.boy = Ac~1.girl &
Bb~1.boy = Bb~1.girl &
Bl~1.boy = Bl~1.girl &
Bf~1.boy = Bf~1.girl &
Bn~1.boy = Bn~1.girl &
Br~1.boy = Br~1.girl &
Bc~1.boy = Bc~1.girl &
A~age.boy < A~age.girl  &
A~pea.boy < A~pea.girl  &
A~B.boy < A~B.girl;
A=~Ab.boy = A=~Ab.girl &
A=~Al.boy = A=~Al.girl &
A=~Af.boy = A=~Af.girl &
A=~An.boy = A=~An.girl &
A=~Ar.boy = A=~Ar.girl &
A=~Ac.boy = A=~Ac.girl &
B=~Bb.boy = B=~Bb.girl &
B=~Bl.boy = B=~Bl.girl &
B=~Bf.boy = B=~Bf.girl &
B=~Bn.boy = B=~Bn.girl &
B=~Br.boy = B=~Br.girl &
B=~Bc.boy = B=~Bc.girl &
Ab~1.boy = Ab~1.girl &
Al~1.boy = Al~1.girl &
Af~1.boy = Af~1.girl &
An~1.boy = An~1.girl &
Ar~1.boy = Ar~1.girl &
Ac~1.boy = Ac~1.girl &
Bb~1.boy = Bb~1.girl &
Bl~1.boy = Bl~1.girl &
Bf~1.boy = Bf~1.girl &
Bn~1.boy = Bn~1.girl &
Br~1.boy = Br~1.girl &
Bc~1.boy = Bc~1.girl &
A~age.boy = A~age.girl  &
A~pea.boy < A~pea.girl  &
A~B.boy < A~B.girl;
A=~Ab.boy = A=~Ab.girl &
A=~Al.boy = A=~Al.girl &
A=~Af.boy = A=~Af.girl &
A=~An.boy = A=~An.girl &
A=~Ar.boy = A=~Ar.girl &
A=~Ac.boy = A=~Ac.girl &
B=~Bb.boy = B=~Bb.girl &
B=~Bl.boy = B=~Bl.girl &
B=~Bf.boy = B=~Bf.girl &
B=~Bn.boy = B=~Bn.girl &
B=~Br.boy = B=~Br.girl &
B=~Bc.boy = B=~Bc.girl &
Ab~1.boy = Ab~1.girl &
Al~1.boy = Al~1.girl &
Af~1.boy = Af~1.girl &
An~1.boy = An~1.girl &
Ar~1.boy = Ar~1.girl &
Ac~1.boy = Ac~1.girl &
Bb~1.boy = Bb~1.girl &
Bl~1.boy = Bl~1.girl &
Bf~1.boy = Bf~1.girl &
Bn~1.boy = Bn~1.girl &
Br~1.boy = Br~1.girl &
Bc~1.boy = Bc~1.girl &
A~age.boy = A~age.girl  &
A~pea.boy = A~pea.girl  &
A~B.boy < A~B.girl;
A=~Ab.boy = A=~Ab.girl &
A=~Al.boy = A=~Al.girl &
A=~Af.boy = A=~Af.girl &
A=~An.boy = A=~An.girl &
A=~Ar.boy = A=~Ar.girl &
A=~Ac.boy = A=~Ac.girl &
B=~Bb.boy = B=~Bb.girl &
B=~Bl.boy = B=~Bl.girl &
B=~Bf.boy = B=~Bf.girl &
B=~Bn.boy = B=~Bn.girl &
B=~Br.boy = B=~Br.girl &
B=~Bc.boy = B=~Bc.girl &
Ab~1.boy = Ab~1.girl &
Al~1.boy = Al~1.girl &
Af~1.boy = Af~1.girl &
An~1.boy = An~1.girl &
Ar~1.boy = Ar~1.girl &
Ac~1.boy = Ac~1.girl &
Bb~1.boy = Bb~1.girl &
Bl~1.boy = Bl~1.girl &
Bf~1.boy = Bf~1.girl &
Bn~1.boy = Bn~1.girl &
Br~1.boy = Br~1.girl &
Bc~1.boy = Bc~1.girl &
A~age.boy = A~age.girl  &
A~pea.boy = A~pea.girl  &
A~B.boy = A~B.girl"

set.seed(100)
y2 <- bain(fit3, hypotheses32, standardized = TRUE)

# HERE FOLLOWS THE CALL TO BAIN DEFAULT

# determine the sample size per group
ngroup3 <- table(sesameCFA$sex)

# obtain the standardized estimates per group.
PE3 <- lavaan::parameterEstimates(fit3, standardized = TRUE)
# here, we only need the rows that correspond to regressions (ie op == "~") and factor loadings
# (ie op == "=~"). Note that, contained in estimate are first the factor loadings of Group 1,
# then the regression coefficients of Group 1 the the factor intercepts of Group 1, followed by the
# factor loadings, regression coefficients, and factor intercepts of Group 2.
estimate3 <- PE3[ PE3$op == "=~" |PE3$op == "~" |  PE3$op == "~1", "std.all"]
# below the intercepts of before, after, age and pea are dropped for both Group 1 and Group 2
estimate3 <- estimate3[c(1:27,32:58)]

# names ending with b are for the boys group, names ending with g are for
# the girls group, subsequently, loadings, regresssion coefficients and intercepts,
# first for the boys, then for the girls
names(estimate3) <- c("A=~Ab.boy", "A=~Al.boy", "A=~Af.boy", "A=~An.boy", "A=~Ar.boy", "A=~Ac.boy","B=~Bb.boy", "B=~Bl.boy", "B=~Bf.boy", "B=~Bn.boy", "B=~Br.boy", "B=~Bc.boy",
                      "A~B.boy", "A~age.boy", "A~pea.boy",
                      "Ab~1.boy","Al~1.boy", "Af~1.boy", "An~1.boy", "Ar~1.boy", "Ac~1.boy","Bb~1.boy","Bl~1.boy", "Bf~1.boy", "Bn~1.boy", "Br~1.boy", "Bc~1.boy",
                      "A=~Ab.girl", "A=~Al.girl", "A=~Af.girl", "A=~An.girl", "A=~Ar.girl", "A=~Ac.girl","B=~Bb.girl", "B=~Bl.girl", "B=~Bf.girl", "B=~Bn.girl", "B=~Br.girl", "B=~Bc.girl",
                      "A~B.girl", "A~age.girl", "A~pea.girl",
                      "Ab~1.girl","Al~1.girl", "Af~1.girl", "An~1.girl", "Ar~1.girl", "Ac~1.girl","Bb~1.girl","Bl~1.girl", "Bf~1.girl", "Bn~1.girl", "Br~1.girl", "Bc~1.girl")

# obtain the covariance matrix of the standardized parameters for the boys
boy_covs <- gsub("\\.boy", "", names(estimate3)[grepl("\\.boy$", names(estimate3))])
boy_covs <- match(boy_covs, colnames(lavInspect(fit3, "vcov.std.all")))
covarianceb3 <- lavInspect(fit3, "vcov.std.all")[boy_covs, boy_covs]

# obtain the covariance matrix of the standardized parameters for the girls
girl_covs <- gsub("girl$", "g2", names(estimate3)[grepl("\\.girl$", names(estimate3))])
girl_covs <- match(girl_covs, colnames(lavInspect(fit3, "vcov.std.all")))
covarianceg3 <- lavInspect(fit3, "vcov.std.all")[girl_covs, girl_covs]

# create a list of covariance matrices
covariance3 <- list(covarianceb3, covarianceg3)

# test the hypothesis of measurement invariance

set.seed(100)
z1 <- bain(estimate3, hypotheses31, n = ngroup3, Sigma = covariance3,
                group_parameters = 27, joint_parameters = 0)
y1
sz1 <- summary(z1, ci = 0.90)

# MODIFY DO THIS CONDITIONALLY ON THE HYPOTHESIS OF MEASUREMENT INVARIANCE
# test hypotheses with respect to the regression coefficients


# note that in the call to bain group_parameters now denotes the number
# of parameters per group
set.seed(100)
z2<- bain(estimate3, hypotheses32, n = ngroup3, Sigma = covariance3,
                group_parameters = 27, joint_parameters = 0)

# HERE FOLLOWS THE CHECK IF S3 GIVES THE SAME RESULTS AS DEFAULT

# TEST RESULTS first hypotheses

test_that("Bain mutual", {expect_equal(y1$fit$Fit , z1$fit$Fit)})
test_that("Bain mutual", {expect_equal(y1$fit$Com , z1$fit$Com)})
test_that("Bain mutual", {expect_equal(y1$independent_restrictions, z1$independent_restrictions)})
test_that("Bain mutual", {expect_equal(y1$b, z1$b)})
test_that("Bain mutual", {expect_equal(as.vector(y1$posterior), as.vector(z1$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(y1$prior), as.vector(z1$prior))})
test_that("Bain mutual", {expect_equal(y1$fit$BF,z1$fit$BF)})
test_that("Bain mutual", {expect_equal(y1$fit$PMPb , z1$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(y1$BFmatrix)), as.vector(t(z1$BFmatrix)))})

# TEST SUMMARY first hypotheses

test_that("summary", {expect_equal(sy1$Estimate , sz1$Estimate)})
test_that("summary", {expect_equal(sy1$n , sz1$n)})
test_that("summary", {expect_equal(sy1$lb , sz1$lb)})
test_that("summary", {expect_equal(sy1$ub , sz1$ub)})

# TEST RESULTS second hypotheses

test_that("Bain mutual", {expect_equal(y2$fit$Fit , z2$fit$Fit)})
test_that("Bain mutual", {expect_equal(y2$fit$Com , z2$fit$Com)})
test_that("Bain mutual", {expect_equal(y2$independent_restrictions, z2$independent_restrictions)})
test_that("Bain mutual", {expect_equal(y2$b, z2$b)})
test_that("Bain mutual", {expect_equal(as.vector(y2$posterior), as.vector(z2$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(y2$prior), as.vector(z2$prior))})
test_that("Bain mutual", {expect_equal(y2$fit$BF,z2$fit$BF)})
test_that("Bain mutual", {expect_equal(y2$fit$PMPb , z2$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(y2$BFmatrix)), as.vector(t(z2$BFmatrix)))})
