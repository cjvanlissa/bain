# ===================================================
# TEST1: FRACTION FOR BAIN DEFAULT
# ===================================================

library(bain)
library(testthat)

sesamesim$site <- as.factor(sesamesim$site)
anov <- lm(sesamesim$postnumb~sesamesim$site-1)

# ANOVA VIA BAIN_DEFAULT with fraction = 4

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
        site2>(site1,site3,site4,site5)",n=samp,Sigma=covmat,group_parameters=1,joint_parameters = 0, fraction = 4)

# ANOVA VIA BAIN_DEFAULT with N/4

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
z<-bain(est,"site1=site2=site3=site4=site5;
        site2>site5>site1>site3=site4;
        site1=site2>site3=site4>site5;
        site1<site2>site3<site4>site5;
        site1=site5>site3=site4<site2;
        site2>site3>site4;
        (site1,site2,site5)>(site3,site4);
        site2>(site1,site3,site4,site5)",n=samp/4,Sigma=covmat,group_parameters=1,joint_parameters = 0)

# TESTING BOTH DEFAULT ANALYSES AGAINST EACH OTHER

test_that("Bain mutual", {expect_equal(y$fit$Fit , z$fit$Fit)})
test_that("Bain mutual", {expect_equal(y$fit$Com , z$fit$Com)})
test_that("Bain mutual", {expect_equal(y$independent_restrictions, z$independent_restrictions)})
test_that("Bain mutual", {expect_equal(y$b, z$b)})
test_that("Bain mutual", {expect_equal(as.vector(y$posterior), as.vector(z$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(y$prior), as.vector(z$prior))})
test_that("Bain mutual", {expect_equal(y$fit$BF,z$fit$BF)})
test_that("Bain mutual", {expect_equal(y$fit$PMPb , z$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(y$BFmatrix)), as.vector(t(z$BFmatrix)))})



# ===================================================
# TEST2: FRACTION FOR ANOVA
# ===================================================

library(bain)
library(testthat)

sesamesim$site <- as.factor(sesamesim$site)
anov <- lm(sesamesim$postnumb~sesamesim$site-1)
set.seed(100)
z<-bain(anov, "site1=site2=site3=site4=site5;
      site2>site5>site1>site3=site4;
     site1=site2>site3=site4>site5;
     site1<site2>site3<site4>site5;
     site1=site5>site3=site4<site2;
     site2>site3>site4;
     (site1,site2,site5)>(site3,site4);
     site2>(site1,site3,site4,site5)", fraction = 4)

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
     site2>(site1,site3,site4,site5)",n=samp,Sigma=covmat,group_parameters=1,joint_parameters = 0, fraction = 4)

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

# ===================================================
# TEST3: FRACTION FOR ANCOVA
# ===================================================

library(bain)
library(testthat)

# ANCOVA VIA LM OBJECT
sesamesim$site <- as.factor(sesamesim$site)
ancov <- lm(postnumb ~ site + prenumb + peabody -1, data = sesamesim)

set.seed(100)
y<-bain(ancov, "site1=site2=site3=site4=site5;site2 > site5 > site3 > site1 >site4;", fraction = 3.5)

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
z<-bain(est,"v.1=v.2=v.3=v.4=v.5;v.2 > v.5 > v.3 > v.1 >v.4;",n=samp/3.5,Sigma=covariances,group_parameters=1,joint_parameters = 2)

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

# ===================================================
# TEST4: FRACTION FOR TTESTS
# ===================================================

library(bain)
library(testthat)

# THE ONE SAMPLE T-TEST WITH A T.TEST OBJECT

x<-sesamesim$postnumb
ttest <- t_test(x)
set.seed(100)
z <- bain(ttest, "x=30; x>30; x<30", fraction =4)

# THE ONE SAMPLE T-TEST WITH BAIN DEFAULT

cov1<-list(matrix(c(sd(x)^2/length(x)),1,1))
estimate<-mean(x)
names(estimate)<-c("m1")
set.seed(100)
zd <-bain(estimate,"m1=30;m1>30;m1<30",n=length(x)/4,Sigma=cov1,group_parameters=1,joint_parameters = 0)

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



x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]
ttest <- t_test(x,y,paired = FALSE, var.equal = FALSE)
set.seed(100)
z <- bain(ttest, "x=y; x>y; x<y", fraction =3.5)

# THE INDEPENDENT GROUPS WELCH TEST WITH BAIN DEFAULT

cov1<-list(matrix(c(sd(x)^2/length(x)),1,1),matrix(c(sd(y)^2/length(y)),1,1))
estimate<-c(mean(x),mean(y))
samp <- c(length(x),length(y))
names(estimate)<-c("m1","m2")
set.seed(100)
zd <-bain(estimate,"m1=m2; m1>m2; m1<m2",n=samp/3.5,Sigma=cov1,group_parameters=1,joint_parameters = 0)


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



x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]

ttest <- t_test(x,y,paired = FALSE, var.equal = TRUE)
set.seed(100)
z <- bain(ttest, "x=y; x>y; x<y", fraction = 3)

# THE INDEPENDENT GROUPS T-TEST WITH BAIN DEFAULT

pooled <- ((length(x)-1)*sd(x)^2+(length(y)-1)*sd(y)^2)/(length(x)-1+length(y)-1)
cov1<-list(matrix(c(pooled),1,1)/length(x),matrix(c(pooled),1,1)/length(y))
estimate<-c(mean(x),mean(y))
samp <- c(length(x),length(y))
names(estimate)<-c("m1","m2")
set.seed(100)
zd <-bain(estimate,"m1=m2; m1>m2; m1<m2",n=samp/3,Sigma=cov1,group_parameters=1,joint_parameters = 0)

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

sesamesim$sex<-as.factor(sesamesim$sex)
ttest <- t_test(postnumb~sex,data=sesamesim,paired = FALSE, var.equal = TRUE)
set.seed(100)
zh<-bain(ttest, "group1=group2; group1>group2; group1<group2", fraction = 3)

test_that("Bain mutual", {expect_equal(zd$fit$Fit , zh$fit$Fit)})
test_that("Bain mutual", {expect_equal(zd$fit$Com , zh$fit$Com)})
test_that("Bain mutual", {expect_equal(zd$b, zh$b)})
test_that("Bain mutual", {expect_equal(zd$independent_restrictions, zh$independent_restrictions)})
test_that("Bain mutual", {expect_equal(as.vector(zd$posterior), as.vector(zh$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(zd$prior), as.vector(zh$prior))})
test_that("Bain mutual", {expect_equal(zd$fit$BF,zh$fit$BF)})
test_that("Bain mutual", {expect_equal(zd$fit$PMPb , zh$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(zd$BFmatrix)), as.vector(t(zh$BFmatrix)))})




# =================================================================================================

# THE PAIRED SAMPLES T-TEST WITH A T.TEST OBJECT



x<-sesamesim$prenumb
y<-sesamesim$postnumb

ttest <- t_test(x,y,paired = TRUE)
set.seed(100)
z <- bain(ttest, "difference=0; difference>0; difference<0", fraction =4)

# THE PAIRED SAMPLES T-TEST WITH BAIN DEFAULT

d <- x - y
cov1<-list(matrix(c(sd(d)^2/length(d)),1,1))
estimate<-mean(d)
names(estimate)<-c("dd")
set.seed(100)
zd <-bain(estimate,"dd=0;dd>0;dd<0",n=length(d)/4,Sigma=cov1,group_parameters=1,joint_parameters = 0)

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



x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]

ttest <- t_test(x,y,paired = FALSE, var.equal = TRUE)
set.seed(100)
z <- bain(ttest, "x - y > -1 & x - y < 1", fraction =5)

# THE INDEPENDENT GROUPS T-TEST WITH BAIN DEFAULT

pooled <- ((length(x)-1)*sd(x)^2+(length(y)-1)*sd(y)^2)/(length(x)-1+length(y)-1)
cov1<-list(matrix(c(pooled),1,1)/length(x),matrix(c(pooled),1,1)/length(y))
estimate<-c(mean(x),mean(y))
samp <- c(length(x),length(y))
names(estimate)<-c("m1","m2")
set.seed(100)
zd <-bain(estimate,"m1 - m2 > -1 & m1 - m2 < 1",n=samp/5,Sigma=cov1,group_parameters=1,joint_parameters = 0)


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

# T.TEST VARIATIONS: T.TEST WITH AN ACTIVE ALTERNATIVE OPTION



sesamesim$sex <- as.factor(sesamesim$sex)
x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]
ttest <- t_test(x,y,paired = FALSE, var.equal = FALSE,alternative = c("less"))
set.seed(100)
z1 <- bain(ttest, "x=y; x>y; x<y", fraction =1)
x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]
ttest <- t_test(x,y,paired = FALSE, var.equal = FALSE)
set.seed(100)
z2 <- bain(ttest, "x=y; x>y; x<y", fraction =1)

# TESTING BAIN T.TEST AND T.TEST WITH ALTERNATIVE OPTION VERSUS EACH OTHER

test_that("Bain mutual", {expect_equal(z1$fit$Fit , z2$fit$Fit)})
test_that("Bain mutual", {expect_equal(z1$fit$Com , z2$fit$Com)})
test_that("Bain mutual", {expect_equal(z1$independent_restrictions, z2$independent_restrictions)})
test_that("Bain mutual", {expect_equal(z1$b, z2$b)})
test_that("Bain mutual", {expect_equal(as.vector(z1$posterior), as.vector(z2$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(z1$prior), as.vector(z2$prior))})
test_that("Bain mutual", {expect_equal(z1$fit$BF,z2$fit$BF)})
test_that("Bain mutual", {expect_equal(z1$fit$PMPb , z2$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(z1$BFmatrix)), as.vector(t(z2$BFmatrix)))})

# T.TEST VARIATIONS: T.TEST WITH AN ACTIVE MU OPTION



sesamesim$sex <- as.factor(sesamesim$sex)
x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]
ttest <- t_test(x,y,paired = FALSE, var.equal = FALSE,mu=50)
set.seed(100)
z1 <- bain(ttest, "x=y; x>y; x<y", fraction =2)

x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]
ttest <- t_test(x,y,paired = FALSE, var.equal = FALSE)
set.seed(100)
z2 <- bain(ttest, "x=y; x>y; x<y", fraction =2)

# TESTING BAIN T.TEST AND T.TEST WITH MU OPTION VERSUS EACH OTHER

test_that("Bain mutual", {expect_equal(z1$fit$Fit , z2$fit$Fit)})
test_that("Bain mutual", {expect_equal(z1$fit$Com , z2$fit$Com)})
test_that("Bain mutual", {expect_equal(z1$independent_restrictions, z2$independent_restrictions)})
test_that("Bain mutual", {expect_equal(z1$b, z2$b)})
test_that("Bain mutual", {expect_equal(as.vector(z1$posterior), as.vector(z2$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(z1$prior), as.vector(z2$prior))})
test_that("Bain mutual", {expect_equal(z1$fit$BF,z2$fit$BF)})
test_that("Bain mutual", {expect_equal(z1$fit$PMPb , z2$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(z1$BFmatrix)), as.vector(t(z2$BFmatrix)))})

# ===================================================
# TEST5: FRACTION FOR REGRESSION
# ===================================================

library(bain)
library(testthat)

regr <- lm(postnumb ~ prenumb + funumb + peabody, sesamesim)
regr$call$formula
# UNSTANDARDIZED REGRESSION USING AN LM OBJECT
set.seed(100)
z<-bain(regr,"pre=fu=pea;pea > fu > pre; pre>fu>pea", standardize = FALSE, fraction = 4.5)

# UNSTANDARDIZED REGRESSION USING BAIN DEFAULT

samp <- dim(sesamesim)[1]
regr <- lm(postnumb ~ prenumb + funumb + peabody, data = sesamesim)
est <- coef(regr)[-1]
cov <- vcov(regr)[-1, -1]
names(est) <- c("pre", "fu", "pea")
set.seed(100)
y<-bain(est,"pre=fu=pea;pea > fu > pre; pre>fu>pea",n=samp/4.5,Sigma=cov,group_parameters=0,joint_parameters = 3)

# HIERBOVEN VIA JOINT ZONDER LIST - HIERONDER VIA GROUP EN LIST

cov <- list(cov)
set.seed(100)
y2<-bain(est,"pre=fu=pea;pea > fu > pre; pre>fu>pea",n=samp/4.5,Sigma=cov,group_parameters=3,joint_parameters = 0)


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














