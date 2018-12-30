# TESTING ANOVA VIA LM AND DEFAULT VERSUS EACH OTHER

# ANOVA VIA LM OBJECT 

rm(list=ls())
library(testthat)
library(bain)

sesamesim$site <- as.factor(sesamesim$site)
anov <- lm(sesamesim$postnumb~sesamesim$site-1)
anov <- label_estimates(anov, c("site1", "site2", "site3","site4","site5"))
set.seed(100)
z<-bain(anov, "site1=site2=site3=site4=site5;
      site2>site5>site1>site3=site4;
     site1=site2>site3=site4>site5;
     site1<site2>site3<site4>site5;
     site1=site5>site3=site4<site2;
     site2>site3>site4;
     (site1,site2,site5)>(site3,site4);
     site2>(site1,site3,site4,site5)")

# ANOVA VIA BAIN_DEFAULT 

prepSesame <- lm(postnumb~site-1,sesamesim)
est <- coef(prepSesame)

samp <- table(sesamesim$site)

var <- summary(prepSesame)$sigma**2   

cov1 <- var/samp[1]   
cov2 <- var/samp[2]  
cov3 <- var/samp[3]  
cov4 <- var/samp[4]  
cov5 <- var/samp[5]  

cov1 <- matrix(cov1, nrow=1, ncol=1)
cov2 <- matrix(cov2, nrow=1, ncol=1)
cov3 <- matrix(cov3, nrow=1, ncol=1)
cov4 <- matrix(cov4, nrow=1, ncol=1)
cov5 <- matrix(cov5, nrow=1, ncol=1)

covmat <- list(cov1, cov2, cov3, cov4, cov5)

set.seed(100)
y<-bain(est,"site1=site2=site3=site4=site5;
      site2>site5>site1>site3=site4;
     site1=site2>site3=site4>site5;
     site1<site2>site3<site4>site5;
     site1=site5>site3=site4<site2;
     site2>site3>site4;
     (site1,site2,site5)>(site3,site4);
     site2>(site1,site3,site4,site5)",n=samp,Sigma=covmat,group_parameters=1,joint_parameters = 0)

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




