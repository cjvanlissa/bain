# Extracted from test-baindefault.R:101

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "bain", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
data(sesamesim)
estimate <- c(1,1)
names(estimate)<-c("a", "b")
sampN<- 40
cov <- matrix(c(1,0,0,1), nrow=2,ncol=2)
set.seed(100)
y<-bain(estimate,"a-b = 0 & b  > 0",n=sampN,Sigma=cov,group_parameters=0,joint_parameters = 2)
x<-rnorm(100000,1,.707)
for (i in 1:100000){
  if (x[i] > 0){x[i]=1} else {x[i]<-0}
}
estimate <- c(0,4)
names(estimate)<-c("a", "b")
sampN<- 40
cov <- matrix(c(1,0,0,1), nrow=2,ncol=2)
set.seed(100)
y<-bain(estimate,"a-b = 0 & b  > 0",n=sampN,Sigma=cov,group_parameters=0,joint_parameters = 2)
x<-rnorm(100000,2,.707)
for (i in 1:100000){
  if (x[i] > 0){x[i]=1} else {x[i]<-0}
}
estimate <- c(0,0,0,0,0,0)
names(estimate)<-c("a", "b","c", "d", "e", "f")
sampN<- 100
cov <- matrix(c(1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1),nrow=6,ncol=6)
set.seed(100)
y<-bain(estimate,"a=0 & b=0 & c=0 & d=0 & e=0 & f=0; a<0 & b=0 & c<0 & d=0 & e<0 & f=0;a<0 & b>0 & c<0 & d>0 & e<0 & f>0",n=sampN,Sigma=cov,group_parameters=0,joint_parameters = 6)
sigmax <- matrix(c(1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1),nrow=6,ncol=6)
x <- c(0,0,0,0,0,0)
meanx <- c(0,0,0,0,0,0)
sigmaz <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)
z <- c(0,0,0)
meanz <- c(0,0,0)

# test -------------------------------------------------------------------------
expect_equal(y$fit$Com_in[1:4], c(1,.125,.015625,NA))
