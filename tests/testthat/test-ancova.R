rm(list=ls())

# ANCOVA VIA LM OBJECT

sesamesim$site <- as.factor(sesamesim$site)
ancov <- lm(postnumb ~ site + prenumb + peabody -1, data = sesamesim)
ancov <- label_estimates(ancov, c("v.1", "v.2", "v.3","v.4", "v.5", "pre", "pea"))
set.seed(100)
y<-bain(ancov, "v.1=v.2=v.3=v.4=v.5;v.2 > v.5 > v.3 > v.1 >v.4;")

# ANCOVA VIA BAIN_DEFAULT

sesamesim$prenumb <- sesamesim$prenumb-mean(sesamesim$prenumb)
sesamesim$peabody <- sesamesim$peabody-mean(sesamesim$peabody)

ancov <- lm(postnumb ~ site + prenumb + peabody -1, data = sesamesim)
est <- coef(ancov)
samp <- table(sesamesim$site)
prep.var <- (summary(ancov)$sigma)**2

cat1 <- subset(cbind(sesamesim$site,sesamesim$prenumb,sesamesim$peabody), sesamesim$site == 1)
cat1[,1] <- 1
cat1 <- as.matrix(cat1)
cov1 <- prep.var * solve(t(cat1) %*% cat1)

cat2 <- subset(cbind(sesamesim$site,sesamesim$prenumb,sesamesim$peabody), sesamesim$site == 2)
cat2[,1] <- 1
cat2 <- as.matrix(cat2)
cov2 <- prep.var * solve(t(cat2) %*% cat2)

cat3 <- subset(cbind(sesamesim$site,sesamesim$prenumb,sesamesim$peabody), sesamesim$site == 3)
cat3[,1] <- 1
cat3 <- as.matrix(cat3)
cov3 <- prep.var * solve(t(cat3) %*% cat3)

cat4 <- subset(cbind(sesamesim$site,sesamesim$prenumb,sesamesim$peabody), sesamesim$site == 4)
cat4[,1] <- 1
cat4 <- as.matrix(cat4)
cov4 <- prep.var * solve(t(cat4) %*% cat4)

cat5 <- subset(cbind(sesamesim$site,sesamesim$prenumb,sesamesim$peabody), sesamesim$site == 5)
cat5[,1] <- 1
cat5 <- as.matrix(cat5)
cov5 <- prep.var * solve(t(cat5) %*% cat5)

covariances <- list(cov1, cov2, cov3, cov4,cov5)

names(est)<- c("v.1", "v.2", "v.3", "v.4","v.5", "pre", "pea")
set.seed(100)
z<-bain(est,"v.1=v.2=v.3=v.4=v.5;v.2 > v.5 > v.3 > v.1 >v.4;",n=samp,Sigma=covariances,group_parameters=1,joint_parameters = 2)

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



# TESTING ANCOVA WITH RESTRICTIONS ON THE COVARIATES

# ==================================================================
# DE NAMEN VAN DE COVARIATEN WORDEN NIET GERESET
# ALS DE ORIGINELE NAMEN GEBRUIKT WORDEN WERKT E.E.A.
# ==================================================================

rm(list=ls())

sesamesim$sex <- as.factor(sesamesim$sex)
ancov <- lm(postnumb ~ sex + prenumb + peabody -1, data = sesamesim)
coef(ancov)
ancov <- label_estimates(ancov, c("a", "b","pre", "pea"))
set.seed(100)
# z<-bain(ancov, " pre > 0 &  pea > 0")
z<-bain(ancov, " a = b & prenumb > 0 &  peabody > 0")

sesamesim$prenumb <- sesamesim$prenumb-mean(sesamesim$prenumb)
sesamesim$peabody <- sesamesim$peabody-mean(sesamesim$peabody)

est <- coef(ancov)
names(est) <- c("a", "b","pre", "pea")
samp <- table(sesamesim$sex)
prep.var <- (summary(ancov)$sigma)**2

cat1 <- subset(cbind(sesamesim$sex,sesamesim$prenumb,sesamesim$peabody), sesamesim$sex == 1)
cat1[,1] <- 1
cat1 <- as.matrix(cat1)
cov1 <- prep.var * solve(t(cat1) %*% cat1)

cat2 <- subset(cbind(sesamesim$sex,sesamesim$prenumb,sesamesim$peabody), sesamesim$sex == 2)
cat2[,1] <- 1
cat2 <- as.matrix(cat2)
cov2 <- prep.var * solve(t(cat2) %*% cat2)

covariances <- list(cov1, cov2)

set.seed(100)
y<-bain(est,"a = b & pre>0 & pea>0",n=samp,Sigma=covariances,group_parameters=1,joint_parameters = 2)

# TESTING BAIN LM AND DEFAULT VERSUS EACH OTHER

test_that("Bain mutual", {expect_equal(y$fit$Fit , z$fit$Fit, tolerance = .001)})
test_that("Bain mutual", {expect_equal(y$fit$Com , z$fit$Com, tolerance = .001)})
test_that("Bain mutual", {expect_equal(y$independent_restrictions, z$independent_restrictions)})
test_that("Bain mutual", {expect_equal(y$b, z$b)})
test_that("Bain mutual", {expect_equal(as.vector(y$posterior), as.vector(z$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(y$prior), as.vector(z$prior))})
test_that("Bain mutual", {expect_equal(y$fit$PMPb , z$fit$PMPb, tolerance =.001)})
test_that("Bain mutual", {expect_equal(as.vector(t(y$BFmatrix)), as.vector(t(z$BFmatrix)))})





