# BELOW THE T.TEST INPUT FOR BAIN IS TESTED

# ===============================================================================================

# THE ONE SAMPLE T-TEST WITH A T.TEST OBJECT

rm(list=ls())
library(testthat)
library(bain)

x<-sesamesim$postnumb
ttest <- t.test(x)
get_estimates(ttest)
ttest <- label_estimates(ttest, c("m1"))
set.seed(100)
z <- bain(ttest, "m1=30; m1>30; m1<30")

# THE ONE SAMPLE T-TEST WITH BAIN DEFAULT

cov1<-list(matrix(c(sd(x)^2/length(x)),1,1))
estimate<-mean(x) 
names(estimate)<-c("m1")
set.seed(100)
zd <-bain(estimate,"m1=30;m1>30;m1<30",n=length(x),Sigma=cov1,group_parameters=1,joint_parameters = 0)

# TESTING BAIN T.TEST AND DEFAULT VERSUS EACH OTHER

test_that("Bain mutual", {expect_equal(zd$fit$Fit , z$fit$Fit)})
test_that("Bain mutual", {expect_equal(zd$fit$Com , z$fit$Com)})
test_that("Bain mutual", {expect_equal(zd$independent_restrictions, z$independent_restrictions)})
test_that("Bain mutual", {expect_equal(zd$b, z$b)})
test_that("Bain mutual", {expect_equal(as.vector(zd$posterior), as.vector(z$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(zd$prior), as.vector(z$prior))})
test_that("Bain mutual", {expect_equal(zd$fit$BF,z$fit$BF)})
test_that("Bain mutual", {expect_equal(zd$fit$PMPb , z$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(zd$BFmatrix)), as.vector(t(z$BFmatrix)))})
# ===============================================================================================

# THE INDEPENDENT GROUPS WELCH TEST WITH A T.TEST OBJECT

rm(list=ls())
library(testthat)
library(bain)

x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]
ttest <- t.test(x,y,paired = FALSE, var.equal = FALSE)
get_estimates(ttest)
ttest <- label_estimates(ttest, c("m1","m2"))
set.seed(100)
z <- bain(ttest, "m1=m2; m1>m2; m1<m2")

# THE INDEPENDENT GROUPS WELCH TEST WITH BAIN DEFAULT

cov1<-list(matrix(c(sd(x)^2/length(x)),1,1),matrix(c(sd(y)^2/length(y)),1,1))
estimate<-c(mean(x),mean(y)) 
samp <- c(length(x),length(y))
names(estimate)<-c("m1","m2")
set.seed(100)
zd <-bain(estimate,"m1=m2; m1>m2; m1<m2",n=samp,Sigma=cov1,group_parameters=1,joint_parameters = 0)


# TESTING BAIN T.TEST AND DEFAULT VERSUS EACH OTHER

test_that("Bain mutual", {expect_equal(zd$fit$Fit , z$fit$Fit)})
test_that("Bain mutual", {expect_equal(zd$fit$Com , z$fit$Com)})
test_that("Bain mutual", {expect_equal(zd$independent_restrictions, z$independent_restrictions)})
test_that("Bain mutual", {expect_equal(zd$b, z$b)})
test_that("Bain mutual", {expect_equal(as.vector(zd$posterior), as.vector(z$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(zd$prior), as.vector(z$prior))})
test_that("Bain mutual", {expect_equal(zd$fit$BF,z$fit$BF)})
test_that("Bain mutual", {expect_equal(zd$fit$PMPb , z$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(zd$BFmatrix)), as.vector(t(z$BFmatrix)))})

#==================================================================================================

# THE INDEPENDENT GROUPS T-TEST WITH A T.TEST OBJECT

rm(list=ls())
library(testthat)
library(bain)

x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]

ttest <- t.test(x,y,paired = FALSE, var.equal = TRUE)
get_estimates(ttest)
ttest <- label_estimates(ttest, c("m1","m2"))
set.seed(100)
z <- bain(ttest, "m1=m2; m1>m2; m1<m2")

# THE INDEPENDENT GROUPS T-TEST WITH BAIN DEFAULT

pooled <- ((length(x)-1)*sd(x)^2+(length(y)-1)*sd(y)^2)/(length(x)-1+length(y)-1)
cov1<-list(matrix(c(pooled),1,1)/length(x),matrix(c(pooled),1,1)/length(y))
estimate<-c(mean(x),mean(y)) 
samp <- c(length(x),length(y))
names(estimate)<-c("m1","m2")
set.seed(100)
zd <-bain(estimate,"m1=m2; m1>m2; m1<m2",n=samp,Sigma=cov1,group_parameters=1,joint_parameters = 0)


# TESTING BAIN T.TEST AND DEFAULT VERSUS EACH OTHER

test_that("Bain mutual", {expect_equal(zd$fit$Fit , z$fit$Fit)})
test_that("Bain mutual", {expect_equal(zd$fit$Com , z$fit$Com)})
test_that("Bain mutual", {expect_equal(zd$independent_restrictions, z$independent_restrictions)})
test_that("Bain mutual", {expect_equal(zd$b, z$b)})
test_that("Bain mutual", {expect_equal(as.vector(zd$posterior), as.vector(z$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(zd$prior), as.vector(z$prior))})
test_that("Bain mutual", {expect_equal(zd$fit$BF,z$fit$BF)})
test_that("Bain mutual", {expect_equal(zd$fit$PMPb , z$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(zd$BFmatrix)), as.vector(t(z$BFmatrix)))})

# =================================================================================================

# THE PAIRED SAMPLES T-TEST WITH A T.TEST OBJECT

rm(list=ls())
library(testthat)
library(bain)

x<-sesamesim$prenumb
y<-sesamesim$postnumb

ttest <- t.test(x,y,paired = TRUE)
get_estimates(ttest)
ttest <- label_estimates(ttest, c("d"))
set.seed(100)
z <- bain(ttest, "d=0; d>0; d<0")

# THE PAIRED SAMPLES T-TEST WITH BAIN DEFAULT

d <- x - y
cov1<-list(matrix(c(sd(d)^2/length(d)),1,1))
estimate<-mean(d) 
names(estimate)<-c("dd")
set.seed(100)
zd <-bain(estimate,"dd=0;dd>0;dd<0",n=length(d),Sigma=cov1,group_parameters=1,joint_parameters = 0)

# TESTING BAIN T.TEST AND DEFAULT VERSUS EACH OTHER

test_that("Bain mutual", {expect_equal(zd$fit$Fit , z$fit$Fit)})
test_that("Bain mutual", {expect_equal(zd$fit$Com , z$fit$Com)})
test_that("Bain mutual", {expect_equal(zd$independent_restrictions, z$independent_restrictions)})
test_that("Bain mutual", {expect_equal(zd$b, z$b)})
test_that("Bain mutual", {expect_equal(as.vector(zd$posterior), as.vector(z$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(zd$prior), as.vector(z$prior))})
test_that("Bain mutual", {expect_equal(zd$fit$BF,z$fit$BF)})
test_that("Bain mutual", {expect_equal(zd$fit$PMPb , z$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(zd$BFmatrix)), as.vector(t(z$BFmatrix)))})

#==================================================================================================

# THE EQUIVALENCE TEST WITH A T.TEST OBJECT

rm(list=ls())
library(testthat)
library(bain)

x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]

ttest <- t.test(x,y,paired = FALSE, var.equal = TRUE)
get_estimates(ttest)
ttest <- label_estimates(ttest, c("m1","m2"))
set.seed(100)
z <- bain(ttest, "m1 - m2 > -1 & m1 - m2 < 1")

# THE INDEPENDENT GROUPS T-TEST WITH BAIN DEFAULT

pooled <- ((length(x)-1)*sd(x)^2+(length(y)-1)*sd(y)^2)/(length(x)-1+length(y)-1)
cov1<-list(matrix(c(pooled),1,1)/length(x),matrix(c(pooled),1,1)/length(y))
estimate<-c(mean(x),mean(y)) 
samp <- c(length(x),length(y))
names(estimate)<-c("m1","m2")
set.seed(100)
zd <-bain(estimate,"m1 - m2 > -1 & m1 - m2 < 1",n=samp,Sigma=cov1,group_parameters=1,joint_parameters = 0)


# TESTING BAIN T.TEST AND DEFAULT VERSUS EACH OTHER

test_that("Bain mutual", {expect_equal(zd$fit$Fit , z$fit$Fit)})
test_that("Bain mutual", {expect_equal(zd$fit$Com , z$fit$Com)})
test_that("Bain mutual", {expect_equal(zd$independent_restrictions, z$independent_restrictions)})
test_that("Bain mutual", {expect_equal(zd$b, z$b)})
test_that("Bain mutual", {expect_equal(as.vector(zd$posterior), as.vector(z$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(zd$prior), as.vector(z$prior))})
test_that("Bain mutual", {expect_equal(zd$fit$BF,z$fit$BF)})
test_that("Bain mutual", {expect_equal(zd$fit$PMPb , z$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(zd$BFmatrix)), as.vector(t(z$BFmatrix)))})

# =================================================================================================




