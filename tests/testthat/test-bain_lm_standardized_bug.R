# TESTS MET REGRESSION STANDARDIZED ALS NIET ALLE PREDICTOREN
# IN DE HYPOTHESE VOORKOMEN

# ===================================================================
# HIERONDER EEN VOORBEELD VAN DE BUG - TEST TWEE VAN DRIE
# ===================================================================
regr <- lm(postnumb ~ prenumb + funumb + peabody, sesamesim)
set.seed(100)
sz<-bain(regr,"pre=fu; fu < pre", standardize = TRUE)

# HIERONDER EEN SETUP DIE WEL WERKT
# EN DE TESTTHAT OM BOVENSTAANDE
# NA REPARATIE MET DE ONDERSTAANDE TE VERGELIJKEN

samp <- dim(sesamesim)[1]
predictors <- cbind(sesamesim$prenumb, sesamesim$funumb, sesamesim$peabody)
int <- seBeta(X = predictors, y = sesamesim$postnumb,
              Nobs = NULL, estimator = "Normal")
est <- int$CIs[, 2]
cov <- int$cov.mat
names(est) <- c("pre", "fu", "pea")
set.seed(100)
sy<-bain(est,"pre=fu; fu < pre",n=samp,Sigma=cov,groups=0,joint_parameters = 3)

test_that("Bain mutual", {expect_equal(sy$fit$Fit , sz$fit$Fit)})
test_that("Bain mutual", {expect_equal(sy$fit$Com , sz$fit$Com)})
test_that("Bain mutual", {expect_equal(sy$independent_restrictions, sz$independent_restrictions)})
test_that("Bain mutual", {expect_equal(sy$b, sz$b)})
test_that("Bain mutual", {expect_equal(as.vector(sy$posterior[1:2,1:2]), as.vector(sz$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(sy$prior[1:2, 1:2]), as.vector(sz$prior))})
test_that("Bain mutual", {expect_equal(sy$fit$BF,sz$fit$BF)})
test_that("Bain mutual", {expect_equal(sy$fit$PMPb , sz$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(sy$BFmatrix)), as.vector(t(sz$BFmatrix)))})

# ===================================================================
# HIERONDER EEN VOORBEELD VAN DE BUG - TEST DRIE VAN VIJF
# ===================================================================
regr <- lm(postnumb ~ prenumb + funumb + peabody + age + sex, sesamesim)
set.seed(100)
sz<-bain(regr,"pre=pea=se; se < pea < pre", standardize = TRUE)

# HIERONDER EEN SETUP DIE WEL WERKT
# EN DE TESTTHAT OM BOVENSTAANDE
# NA REPARATIE MET DE ONDERSTAANDE TE VERGELIJKEN

samp <- dim(sesamesim)[1]
predictors <- cbind(sesamesim$prenumb, sesamesim$funumb, sesamesim$peabody, sesamesim$age, sesamesim$sex)
int <- seBeta(X = predictors, y = sesamesim$postnumb,
              Nobs = NULL, estimator = "Normal")
est <- int$CIs[, 2]
cov <- int$cov.mat
names(est) <- c("pre", "fu", "pea", "age", "sex")
set.seed(100)
sy<-bain(est,"pre=pea=se; se< pea < pre",n=samp,Sigma=cov,groups=0,joint_parameters = 3)


test_that("Bain mutual", {expect_equal(sy$fit$Fit , sz$fit$Fit)})
test_that("Bain mutual", {expect_equal(sy$fit$Com , sz$fit$Com)})
test_that("Bain mutual", {expect_equal(sy$independent_restrictions, sz$independent_restrictions)})
test_that("Bain mutual", {expect_equal(sy$b, sz$b)})
test_that("Bain mutual", {expect_equal(as.vector(sy$posterior[c(1,3,5),c(1,3,5)]), as.vector(sz$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(sy$prior[c(1,3,5),c(1,3,5)]), as.vector(sz$prior))})
test_that("Bain mutual", {expect_equal(sy$fit$BF,sz$fit$BF)})
test_that("Bain mutual", {expect_equal(sy$fit$PMPb , sz$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(sy$BFmatrix)), as.vector(t(sz$BFmatrix)))})

# ===================================================================
# HIERONDER EEN VOORBEELD VAN DE BUG - TEST EEN VAN EEN
# ===================================================================
regr <- lm(postnumb ~ prenumb, sesamesim)
set.seed(100)
sz<-bain(regr,"pre=0;pre>0", standardize = TRUE)

# HIERONDER EEN SETUP DIE WEL WERKT
# EN DE TESTTHAT OM BOVENSTAANDE
# NA REPARATIE MET DE ONDERSTAANDE TE VERGELIJKEN

samp <- dim(sesamesim)[1]
predictors <- cbind(sesamesim$prenumb)
int <- seBeta(X = predictors, y = sesamesim$postnumb,
              Nobs = NULL, estimator = "Normal")
est <- int$CIs[, 2]
cov <- int$cov.mat
names(est) <- c("pre")
set.seed(100)
sy<-bain(est,"pre=0; pre>0",n=samp,Sigma=cov,groups=0,joint_parameters = 3)


test_that("Bain mutual", {expect_equal(sy$fit$Fit , sz$fit$Fit)})
test_that("Bain mutual", {expect_equal(sy$fit$Com , sz$fit$Com)})
test_that("Bain mutual", {expect_equal(sy$independent_restrictions, sz$independent_restrictions)})
test_that("Bain mutual", {expect_equal(sy$b, sz$b)})
test_that("Bain mutual", {expect_equal(as.vector(sy$posterior), as.vector(sz$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(sy$prior), as.vector(sz$prior))})
test_that("Bain mutual", {expect_equal(sy$fit$BF,sz$fit$BF)})
test_that("Bain mutual", {expect_equal(sy$fit$PMPb , sz$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(sy$BFmatrix)), as.vector(t(sz$BFmatrix)))})


