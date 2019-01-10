rm(list=ls())

regr <- lm(postnumb ~ prenumb + funumb + peabody, sesamesim)
regr <- label_estimates(regr, labels = c("i", "pre", "fu","pea"))
regr$call$formula
# UNSTANDARDIZED REGRESSION USING AN LM OBJECT

set.seed(100)
z<-bain(regr,"pre=fu=pea;pea > fu > pre; pre>fu>pea", standardize = FALSE)

# UNSTANDARDIZED REGRESSION USING BAIN DEFAULT

samp <- dim(sesamesim)[1]
regr <- lm(postnumb ~ prenumb + funumb + peabody, data = sesamesim)
est <- coef(regr)[-1]
cov <- vcov(regr)[-1, -1]
names(est) <- c("pre", "fu", "pea")
set.seed(100)
y<-bain(est,"pre=fu=pea;pea > fu > pre; pre>fu>pea",n=samp,Sigma=cov,group_parameters=0,joint_parameters = 3)

# HIERBOVEN VIA JOINT ZONDER LIST - HIERONDER VIA GROUP EN LIST

cov <- list(cov)
set.seed(100)
y2<-bain(est,"pre=fu=pea;pea > fu > pre; pre>fu>pea",n=samp,Sigma=cov,group_parameters=3,joint_parameters = 0)


# TESTING BAIN LM AND DEFAULT VERSUS EACH OTHER

test_that("Bain mutual", {expect_equal(y$fit$Fit , z$fit$Fit)})
test_that("Bain mutual", {expect_equal(y$fit$Com , z$fit$Com)})
test_that("Bain mutual", {expect_equal(y$independent_restrictions, z$independent_restrictions)})
test_that("Bain mutual", {expect_equal(y$b, z$b)})
test_that("Bain mutual", {expect_equal(as.vector(y$posterior), as.vector(z$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(y$prior), as.vector(z$prior))})
test_that("Bain mutual", {expect_equal(y$fit$BF,z$fit$BF)})
test_that("Bain mutual", {expect_equal(y$fit$PMPb , z$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(y$BFmatrix)), as.vector(t(z$BFmatrix)))})

# TESTING BAIN REGRESSION VIA JOINT EN GROUP VERSUS EACH OTHER

test_that("Bain mutual", {expect_equal(y$fit$Fit , y2$fit$Fit)})
test_that("Bain mutual", {expect_equal(y$fit$Com , y2$fit$Com)})
test_that("Bain mutual", {expect_equal(y$independent_restrictions, y2$independent_restrictions)})
test_that("Bain mutual", {expect_equal(y$b, y2$b)})
test_that("Bain mutual", {expect_equal(as.vector(y$posterior), as.vector(y2$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(y$prior), as.vector(y2$prior))})
test_that("Bain mutual", {expect_equal(y$fit$BF,y2$fit$BF)})
test_that("Bain mutual", {expect_equal(y$fit$PMPb , y2$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(y$BFmatrix)), as.vector(t(y2$BFmatrix)))})

# # STANDARDIZED REGRESSION USING AN LM OBJECT
#
# set.seed(100)
# sz<-bain(regr,"pre=fu=pea;pea > fu > pre; pre>fu>pea", standardize = TRUE)
#
# # STANDARDIZED REGRESSION USING BAIN DEFAULT
#
# samp <- dim(sesamesim)[1]
# predictors <- cbind(sesamesim$prenumb, sesamesim$funumb, sesamesim$peabody)
# int <- seBeta(X = predictors, y = sesamesim$postnumb, Nobs = N, estimator = "Normal")
# est <- int$CIs[, 2]
# cov <- int$cov.mat
# names(est) <- c("pre", "fu", "pea")
# set.seed(100)
#
# sy<-bain(est,"pre=fu=pea;pea > fu > pre; pre>fu>pea",n=samp,Sigma=cov,groups=0,joint_parameters = 4)
#
# # TESTING BAIN LM AND DEFAULT VERSUS EACH OTHER
#
# test_that("Bain mutual", {expect_equal(sy$fit$Fit , sz$fit$Fit)})
# test_that("Bain mutual", {expect_equal(sy$fit$Com , sz$fit$Com)})
# test_that("Bain mutual", {expect_equal(sy$independent_restrictions, sz$independent_restrictions)})
# test_that("Bain mutual", {expect_equal(sy$b, sz$b)})
# test_that("Bain mutual", {expect_equal(as.vector(sy$posterior), as.vector(sz$posterior))})
# test_that("Bain mutual", {expect_equal(as.vector(sy$prior), as.vector(sz$prior))})
# test_that("Bain mutual", {expect_equal(sy$fit$BF,sz$fit$BF)})
# test_that("Bain mutual", {expect_equal(sy$fit$PMPb , sz$fit$PMPb)})
# test_that("Bain mutual", {expect_equal(as.vector(t(sy$BFmatrix)), as.vector(t(sz$BFmatrix)))})


# REGRESSION WITH THE INTERCEPT INCLUDED IN THE RESTRICTIONS

rm(list=ls())
library(testthat)
library(bain)

regr <- lm(postnumb ~ prenumb + peabody, sesamesim)
coef(regr)
regr <- label_estimates(regr, c("i", "num","pea"))
set.seed(100)
sz<-bain(regr,"i=5 & num > pea", standardize = FALSE)

samp <- dim(sesamesim)[1]
regr <- lm(postnumb ~ prenumb + peabody, data = sesamesim)
est <- coef(regr)
cov <- vcov(regr)
names(est) <- c("i", "num","pea")
set.seed(100)
sy<-bain(est,"i=5 & num > pea",n=samp,Sigma=cov,groups=0,joint_parameters = 3)


# TESTING BAIN LM AND DEFAULT VERSUS EACH OTHER

test_that("Bain mutual", {expect_equal(sy$fit$Fit , sz$fit$Fit)})
test_that("Bain mutual", {expect_equal(sy$fit$Com , sz$fit$Com)})
test_that("Bain mutual", {expect_equal(sy$independent_restrictions, sz$independent_restrictions)})
test_that("Bain mutual", {expect_equal(sy$b, sz$b)})
test_that("Bain mutual", {expect_equal(as.vector(sy$posterior), as.vector(sz$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(sy$prior), as.vector(sz$prior))})
test_that("Bain mutual", {expect_equal(sy$fit$BF,sz$fit$BF)})
test_that("Bain mutual", {expect_equal(sy$fit$PMPb , sz$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(sy$BFmatrix)), as.vector(t(sz$BFmatrix)))})


# REGRESSION WITH RESTRICTION ON INTERACTION EFFECT
rm(list=ls())
library(testthat)
library(bain)

samp <- dim(sesamesim)[1]
regr <- lm(postnumb ~ prenumb * peabody, sesamesim)
coef(regr)
regr <- label_estimates(regr, c("i", "num","pea", "int"))
set.seed(100)
sz<-bain(regr,"num >0 ", standardize = FALSE)

regr <- lm(postnumb ~ prenumb * peabody, data = sesamesim)
est <- coef(regr)
cov <- vcov(regr)
names(est) <- c("i", "num","pea", "int")
set.seed(100)
sy<-bain(est,"num > 0",n=samp,Sigma=cov,groups=0,joint_parameters = 4)

# TESTING BAIN LM AND DEFAULT VERSUS EACH OTHER

test_that("Bain mutual", {expect_equal(sy$fit$Fit , sz$fit$Fit)})
test_that("Bain mutual", {expect_equal(sy$fit$Com , sz$fit$Com)})
test_that("Bain mutual", {expect_equal(sy$independent_restrictions, sz$independent_restrictions)})
test_that("Bain mutual", {expect_equal(sy$b, sz$b)})
test_that("Bain mutual", {expect_equal(sy$fit$BF,sz$fit$BF)})
test_that("Bain mutual", {expect_equal(sy$fit$PMPb , sz$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(sy$BFmatrix)), as.vector(t(sz$BFmatrix)))})
















