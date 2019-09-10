# BELOW THE T.TEST INPUT FOR BAIN IS TESTED

# ===============================================================================================

# THE ONE SAMPLE T-TEST WITH A T.TEST OBJECT



x<-sesamesim$postnumb
ttest <- t_test(x)
set.seed(100)
z <- bain(ttest, "x=30; x>30; x<30")

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



x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]
ttest <- t_test(x,y,paired = FALSE, var.equal = FALSE)
set.seed(100)
z <- bain(ttest, "x=y; x>y; x<y")

# THE INDEPENDENT GROUPS WELCH TEST WITH BAIN DEFAULT
ttest$v
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

x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]
ttest <- t_test(x,y,paired = FALSE, var.equal = TRUE)
set.seed(100)
z <- bain(ttest, "x=y; x>y; x<y")

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

sesamesim$sex<-as.factor(sesamesim$sex)
ttest <- bain:::t_test_old(postnumb~sex,data=sesamesim,paired = FALSE, var.equal = TRUE)
ttest <- t_test(postnumb~sex,data=sesamesim,paired = FALSE, var.equal = TRUE)
set.seed(100)
zh<-bain(ttest, "group1=group2; group1>group2; group1<group2")

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
z <- bain(ttest, "difference=0; difference>0; difference<0")

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



x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]

ttest <- t_test(x,y,paired = FALSE, var.equal = TRUE)
set.seed(100)
z <- bain(ttest, "x - y > -1 & x - y < 1")

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

# T.TEST VARIATIONS: T.TEST WITH AN ACTIVE ALTERNATIVE OPTION



sesamesim$sex <- as.factor(sesamesim$sex)
x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]
ttest <- t_test(x,y,paired = FALSE, var.equal = FALSE,alternative = c("less"))
set.seed(100)
z1 <- bain(ttest, "x=y; x>y; x<y")
x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]
ttest <- t_test(x,y,paired = FALSE, var.equal = FALSE)
set.seed(100)
z2 <- bain(ttest, "x=y; x>y; x<y")

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
z1 <- bain(ttest, "x=y; x>y; x<y")

x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]
ttest <- t_test(x,y,paired = FALSE, var.equal = FALSE)
set.seed(100)
z2 <- bain(ttest, "x=y; x>y; x<y")

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







