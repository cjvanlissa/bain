# and now with about equality constraints - b, J, fit and complexity

estimate <- c(1,1,1)
names(estimate)<-c("a", "b", "c")
sampN<- 100
cov <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)

set.seed(100)
y<-bain(estimate,"a>-.96 & a < 2.96 & b>-.96 & b < 2.96 & c>-.96 & c < 2.96",n=sampN,Sigma=cov,group_parameters=0,joint_parameters = 3)

test_that("bain default", {expect_equal(y$fit$Fit_in, c(.837375,NA), tolerance = .014)})

# =============================================

estimate <- c(1,1,1)
names(estimate)<-c("a", "b", "c")
sampN<- 100
cov <- matrix(c(.04,0,0,0,.04,0,0,0,.04),nrow=3,ncol=3)

set.seed(100)
y<-bain(estimate,"a>-.96 & a < 2.96 & b>-.96 & b < 2.96 & c>-.96 & c < 2.96",n=sampN,Sigma=cov,group_parameters=0,joint_parameters = 3)

test_that("bain default", {expect_equal(y$fit$Com_in, c(.837375,NA), tolerance = .016)})
test_that("Bain mutual", {expect_equal(y$b,.04)})
test_that("Bain mutual", {expect_equal(y$independent_restrictions,4)})

# ==========================================

estimate<-c(1,2,3)
names(estimate) <- c("a", "b", "c")
sampN <- 100
covariance <- list(matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3))
set.seed(199)
y<-bain(estimate,"a = 1 & b = 2 & c = 3 ",n=sampN,Sigma=covariance,group_parameters=3,joint_parameters = 0)
test_that("Bain mutual", {expect_equal(y$independent_restrictions,3)})

#==============================================================================================
# compute c and f for an about equality constrained hypothesis with bain and with R")
#==============================================================================================

set.seed(124)
x <- rnorm(20,-.3,.7)
y <- rnorm(40,.2,1.3)

#estimate of parameters
estimate<-c(mean(x),mean(y))
names(estimate)<-c("a","b")
sampN<-c(20,40)
cov1<-matrix(c(sd(x)^2/20),1,1)
cov2<-matrix(c(sd(y)^2/40),1,1)
covmat<-list(cov1,cov2)

# testing an about equality constraint

set.seed(100)
y<-bain(estimate,"a-b > -.5 & b - a > -.5",n=sampN,Sigma=covmat,group_parameters=1,joint_parameters = 0)

# x-y has prior mean zero
# x-y has prior variance 1.155 and sd 1.07475

sampc <- rnorm(100000,0,1.07475)
for (i in 1:100000){
  if (sampc[i] < .5 & sampc[i] > -.5) {sampc[i] <-1} else {sampc[i]<- 0}
}

test_that("bain default", {expect_equal(y$fit$Com[1],mean(sampc), tolerance = .01)})

# x-y has posterior mean -.289637 and posterior variance .039 and sd .19728

sampf <- rnorm(100000,-.289637,.19728)
for (i in 1:100000){
  if (sampf[i] < .5 & sampf[i] > -.5) {sampf[i] <-1} else {sampf[i]<- 0}
}

test_that("bain default", {expect_equal(y$fit$Fit[1], mean(sampf), tolerance = .004)})

#==============================================================================================")
# compute c and f for a range constrained hypothesis with bain and with R - a variation of the previous test")
#==============================================================================================")

# specify hypotheses for a range constraints and check

set.seed(100)
z<-bain(estimate,"a-b > -.3 & b - a > 0",n=sampN,Sigma=covmat,group_parameters=1,joint_parameters = 0)

# x-y has prior mean zero
# x-y has prior variance 1.155 and sd 1.0747

sampcc <- rnorm(50000,-.15,1.0747)
for (i in 1:50000){
  if (sampcc[i] < 0 & sampcc[i] > -.3) {sampcc[i] <-1} else {sampcc[i]<- 0}
}

test_that("bain default", {expect_equal(z$fit$Com[1], mean(sampcc), tolerance = .01)})

# x-y has posterior mean -.292 and posterior variance .039 and sd .1975

sampff <- rnorm(50000,-.29,.1975)
for (i in 1:50000){
  if (sampff[i] < 0 & sampff[i] > -.3) {sampff[i] <-1} else {sampff[i]<- 0}
}

test_that("bain default", {expect_equal(z$fit$Fit[1], mean(sampff), tolerance = .01)})

#==============================================================================================")
# compute c and f for a range constrained hypothesis where range is determined by another parameter")
#==============================================================================================")



#estimate of parameters
estimate<-c(2,4)
names(estimate)<-c("a","b")
sampN<-c(20,40)
cov1<-matrix(1,1,1)
cov2<-matrix(1,1,1)
covmat<-list(cov1,cov2)

# specify hypotheses for a range constraints and check

set.seed(100)
y<-bain(estimate,"a  > -b & a < b",n=sampN,Sigma=covmat,group_parameters=1,joint_parameters = 0)

# a and b have prior mean 0
# a has prior variance 20 sd 4.47  and b has prior variance 40 sd 6.32

countc <- 0
sampa <- rnorm(500000,0,4.472136)
sampb <- rnorm(500000,0,6.324555)
for (i in 1:500000){
  if (sampa[i] > -sampb[i] & sampa[i] < sampb[i]) {countc = 1/500000 + countc}
}

test_that("bain default", {expect_equal(y$fit$Com[1], countc, tolerance = .004)})

countf <- 0
sampa <- rnorm(50000,2,1)
sampb <- rnorm(50000,4,1)
for (i in 1:50000){
  if (sampa[i] > -sampb[i] & sampa[i] < sampb[i]) {countf = 1/50000 + countf}
}

test_that("bain default", {expect_equal(y$fit$Fit[1], countf, tolerance = .01)})

# ==============================================================================
# TESTING ABOUT EQUALITY CONSTRAINTS
# ==============================================================================

#estimate of parameters
estimate<-0.006623468
names(estimate)<-c("disp")
sampN<-32
cov1<-matrix(c(2.018646e-06),1,1)
covmat<-list(cov1)

# testing an about equality constraint

set.seed(100)
y<-bain(estimate,"disp < .1 & disp > -.1  ",n=sampN,Sigma=covmat,group_parameters=1,joint_parameters = 0)

# disp has prior mean zero
# disp has prior variance 3.229834e-05 and prior sd 0.005683163

sampc <- rnorm(100000,0,0.005683163)
for (i in 1:100000){
  if (sampc[i] < .1 & sampc[i] > -.1) {sampc[i] <- 1} else {sampc[i]<- 0}
}

test_that("bain default", {expect_equal(y$fit$Com[1],mean(sampc), tolerance = .01)})


# en nu nog een keer met een veel kleinere range vanwege de kleine se van disp

set.seed(100)
y<-bain(estimate,"disp < .001 & disp > -.001  ",n=sampN,Sigma=covmat,group_parameters=1,joint_parameters = 0)

# disp has prior mean zero
# disp has prior variance 3.229834e-05 and prior sd 0.005683163

sampc <- rnorm(100000,0,0.005683163)
for (i in 1:100000){
  if (sampc[i] < .001 & sampc[i] > -.001) {sampc[i] <- 1} else {sampc[i]<- 0}
}

test_that("bain default", {expect_equal(y$fit$Com[1],mean(sampc), tolerance = .01)})



# en nu nog een keer met range met andere prior mean

set.seed(100)
y<-bain(estimate,"disp < .01 & disp > .005  ",n=sampN,Sigma=covmat,group_parameters=1,joint_parameters = 0)

# disp has prior mean zero
# disp has prior variance 3.229834e-05 and prior sd 0.005683163

sampc <- rnorm(100000,.0075,0.005683163)
for (i in 1:100000){
  if (sampc[i] < .01 & sampc[i] > .005) {sampc[i] <- 1} else {sampc[i]<- 0}
}

sampf <- rnorm(100000,0.006623468,0.001420791)
for (i in 1:100000){
  if (sampf[i] < .01 & sampf[i] > .005) {sampf[i] <- 1} else {sampf[i]<- 0}
}


test_that("bain default", {expect_equal(y$fit$Com[1],mean(sampc), tolerance = .01)})
test_that("bain default", {expect_equal(y$fit$Fit[1],mean(sampf), tolerance = .01)})


