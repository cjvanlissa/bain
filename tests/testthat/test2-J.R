# ============================================================================================================
# TESTING ABOUT EQUALITY WITH RESPECT TO "NOT THE FIRST PARAMETER
data(sesamesim)

# USING JOINT_PARAMETERS
samp <- dim(sesamesim)[1]
regr <- lm(postnumb ~ prenumb + funumb + peabody, data = sesamesim)
est <- coef(regr)[-1]
cov <- vcov(regr)[-1, -1]
names(est) <- c("pre", "fu", "pea")
set.seed(100)
y<-bain(est,"pea > .1 & pea < .15",n=samp,Sigma=cov,group_parameters=0,joint_parameters = 3)

# USING GROUP_PARAMETERS
cov <- list(cov)
set.seed(100)
y2<-bain(est,"pea > .1 & pea < .15",n=samp,Sigma=cov,group_parameters=3,joint_parameters = 0)

# THIS ANALYSIS RENDERS THE CORRECT RESULTS:
samp <- dim(sesamesim)[1]
regr <- lm(postnumb ~ prenumb + funumb + peabody, data = sesamesim)
est <- coef(regr)[4]
cov <- vcov(regr)[4,4]
names(est) <- c("pea")
set.seed(100)
z<-bain(est,"pea > .1 & pea < .15",n=samp,Sigma=cov,group_parameters=0,joint_parameters = 3)

# BELOW IT IS TESTED IF THE THREE ANALYSES GIVE THE SAME RESULTS

test_that("bain default", {expect_equal(z$fit$Fit_in, y$fit$Fit_in)})
test_that("bain default", {expect_equal(z$fit$Com_in, y$fit$Com_in)})
test_that("bain default", {expect_equal(z$fit$Fit_in, y2$fit$Fit_in)})
test_that("bain default", {expect_equal(z$fit$Com_in, y2$fit$Com_in)})

# =============================================================
# ANOTHER TEST WITH 0 < c < d < 2 and 0 < c < 2*d < 2

samp <- c(40,40,40,40)
est <- c(1,1,1,1)
names(est) <- c("a","b","c","d")
cov1 <- matrix(1,1,1)
cov2 <- matrix(1,1,1)
cov3 <- matrix(1,1,1)
cov4 <- matrix(1,1,1)
cov <- list(cov1,cov2,cov3,cov4)
set.seed(100)
z<-bain(est,"0 < c < d < 2",n=samp,Sigma=cov,
        group_parameters=1,joint_parameters = 0)

cfit <- rnorm(100000,1,1)
dfit <- rnorm(100000,1,1)
fit <- 0
for (i in 1:100000){
  if (0 < cfit[i] & cfit[i] < dfit[i] & dfit[i] < 2)
    {fit <- fit + .00001}
}

test_that("bain default", {expect_equal(z$fit$Fit_in[1], fit, tolerance = .01)})



samp <- c(40,40,40,40)
est <- c(1,1,1,1)
names(est) <- c("a","b","c","d")
cov1 <- matrix(1,1,1)
cov2 <- matrix(1,1,1)
cov3 <- matrix(1,1,1)
cov4 <- matrix(1,1,1)
cov <- list(cov1,cov2,cov3,cov4)
set.seed(100)
z<-bain(est,"0 < c < 2 * d < 2",n=samp,Sigma=cov,
        group_parameters=1,joint_parameters = 0)

cfit <- rnorm(100000,1,1)
dfit <- rnorm(100000,1,1)
fit <- 0
for (i in 1:100000){
  if (0 < cfit[i] & cfit[i] < 2 * dfit[i] & 2 * dfit[i] < 2)
  {fit <- fit + .00001}
}

test_that("bain default", {expect_equal(z$fit$Fit_in[1], fit, tolerance = .011)})

# below testing the complexity
cfit <- rnorm(100000,1,7.303)
dfit <- rnorm(100000,.5,7.303)
fit <- 0
for (i in 1:100000){
  if (0 < cfit[i] & cfit[i] < 2 * dfit[i] & 2 * dfit[i] < 2)
  {fit <- fit + .00001}
}

test_that("bain default", {expect_equal(z$fit$Com_in[1], fit, tolerance = .004)})



# ==============================================================
# TESTING AN ABOUT EQUALITY THAT IS IN FACT NOT ONE


samp <- c(40,40,40,40)
est <- c(0,0,0,0)
names(est) <- c("a","b","c","d")
cov1 <- matrix(1,1,1)
cov2 <- matrix(1,1,1)
cov3 <- matrix(1,1,1)
cov4 <- matrix(1,1,1)
cov <- list(cov1,cov2,cov3,cov4)
set.seed(100)
z<-bain(est,"c > -d+1 & c < d+1",n=samp,Sigma=cov,
        group_parameters=1,joint_parameters = 0)

cfit <- rnorm(10000,0,1)
dfit <- rnorm(10000,0,1)
fit <- 0
for (i in 1:10000){
  if (cfit[i] > -dfit[i] + 1 & cfit[i] < dfit[i] +1)
  {fit <- fit + .0001}
}

test_that("bain default", {expect_equal(z$fit$Fit_in[1], fit, tolerance = .01)})

# ========================================================================
# TESTING MULTIPLE PAIRWISE ABOUT EQUALITIES
# ============================================================================================================
estimate <- c(0,0,0)
names(estimate)<-c("a", "b", "c")
sampN<- 100
cov <- list(matrix(c(.5,0,0,0,.5,0,0,0,.5),nrow=3,ncol=3))

set.seed(100)
y<-bain(estimate,"a-b>-1.96 & a-b < 1.96 & b-c>-1.96 & b-c < 1.96 & a-c>-1.96 & a-c < 1.96",n=sampN,Sigma=cov,group_parameters=3,joint_parameters = 0)

ns <- 25000
a <- rnorm(ns,0,.7071)
b <- rnorm(ns,0,.7071)
c <- rnorm(ns,0,.7071)
sampf <- 0
for (i in 1:ns){
  if (a[i]-b[i]>-1.96 & a[i]-b[i] < 1.96 & b[i]-c[i]>-1.96 & b[i]-c[i] < 1.96 & a[i]-c[i]>-1.96 & a[i]-c[i] < 1.96 ) {sampf <- sampf + 1/ns}
}

test_that("bain default", {expect_equal(y$fit$Fit_in[1], sampf, tolerance = .014)})

# ===============================================================================

estimate <- c(0,0,0,0)
names(estimate)<-c("extra","a", "b", "c")
sampN<- 100
cov <- list(matrix(c(.5,0,0,0,
                     0,.5,0,0,
                     0,0,.5,0,
                     0,0,0,.5),nrow=4,ncol=4))

set.seed(100)
y<-bain(estimate,"a-b>-1.96 & a-b < 1.96 & b-c>-1.96 & b-c < 1.96 & a-c>-1.96 & a-c < 1.96",n=sampN,Sigma=cov,group_parameters=4,joint_parameters = 0)

ns <- 25000
a <- rnorm(ns,0,.7071)
b <- rnorm(ns,0,.7071)
c <- rnorm(ns,0,.7071)
sampf <- 0
for (i in 1:ns){
  if (a[i]-b[i]>-1.96 & a[i]-b[i] < 1.96 & b[i]-c[i]>-1.96 & b[i]-c[i] < 1.96 & a[i]-c[i]>-1.96 & a[i]-c[i] < 1.96 ) {sampf <- sampf + 1/ns}
}

test_that("bain default", {expect_equal(y$fit$Fit_in[1], sampf, tolerance = .014)})







