# Extracted from test-baindefault.R:339

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
estimate <- c(-1,0,1)
names(estimate)<-c("a", "b","c")
sampN<- 40
cov <- matrix(c(.1,0,0,0,.1,0,0,0,.1), nrow=3,ncol=3)
set.seed(100)
y<-bain(estimate,"a > b & b > c",n=sampN,Sigma=cov,group_parameters=0,joint_parameters = 3)
estimate <- c(0, 0, 0)
names(estimate)<-c("a", "b","c")
sampN<- 40
cov <- matrix(c(1,0,0,0,1,0,0,0,1), nrow=3,ncol=3)
set.seed(10)
y<-bain(estimate,"a > b & b > 0 & a > 0; a > b & b > 0",n=sampN,Sigma=cov,group_parameters=0,joint_parameters = 3)
estimate <- c(.2,0)
names(estimate)<-c("a", "b")
sampN<- c(20,40)
cov1<-matrix(c(.1),1,1)
cov2<-matrix(c(.1),1,1)
covariance<-list(cov1,cov2)
set.seed(10)
y<-bain(estimate,"a =b; a>b; a<b",n=sampN,Sigma=covariance,group_parameters=1,joint_parameters = 0)
regrdata<-c(sesamesim$site,sesamesim$prenumb,sesamesim$peabody,sesamesim$postnumb)
dim(regrdata)<-c(240,4)
colnames(regrdata)<-c("site","prenumb","peabody","postnumb")
regrdata <- as.data.frame(regrdata)
regrdata$prenumb = regrdata$prenumb - mean(regrdata$prenumb)
regrdata$peabody = regrdata$peabody - mean(regrdata$peabody)
regrdata$site <- factor(regrdata$site)
anal <-  lm(postnumb ~ site + prenumb + peabody -1, data=regrdata)
set.seed(100)
y<-bain(anal, "site1=site2=site3=site4=site5")
estimate<-c(0,0,0,0,0,0,0)
names(estimate) <- c("a", "b", "c", "d", "e", "c1", "c2")
cov1 <- matrix(c(3,0,0,
                 0,3,0,
                 0,0,3), nrow=3, ncol=3)
cov2 <- matrix(c(3,0,0,
                 0,3,0,
                 0,0,3), nrow=3, ncol=3)
cov3 <- matrix(c(3,0,0,
                 0,3,0,
                 0,0,3), nrow=3, ncol=3)
cov4 <- matrix(c(3,0,0,
                 0,3,0,
                 0,0,3), nrow=3, ncol=3)
cov5 <- matrix(c(3,0,0,
                 0,3,0,
                 0,0,3), nrow=3, ncol=3)
sampN<-c(20,20,20,20,20)
covariance<-list(cov1,cov2,cov3,cov4,cov5)
set.seed(10)
y<-bain(estimate,"a=b=c=d=e",n=sampN,Sigma=covariance,group_parameters=1,joint_parameters = 2)
in1 <- solve(cov1/y$b[1])
in2 <- solve(cov2/y$b[2])
in3 <- solve(cov3/y$b[3])
in4 <- solve(cov4/y$b[4])
in5 <- solve(cov5/y$b[5])
invcov <- solve(y$prior)
invcov77 <- in1[3,3] + in2[3,3] + in3[3,3] + in4[3,3] + in5[3,3]
invcov67 <- in1[2,3] + in2[2,3] + in3[2,3] + in4[2,3] + in5[2,3]
invcov57 <- in5[1,3]
estimate<-c(0,0,0,0,0,0,0)
names(estimate) <- c("a", "b", "c", "d", "e", "c1", "c2")
cov1 <- matrix(c(3,.5,0,
                 .5,3,0,
                 0,0,3), nrow=3, ncol=3)
cov2 <- matrix(c(1,.6,.7,
                 .6,1,.5,
                 .7,.5,1), nrow=3, ncol=3)
cov3 <- matrix(c(3,0,.2,
                 0,3,0,
                 .2,0,3), nrow=3, ncol=3)
cov4 <- matrix(c(3,0,0,
                 0,3,0,
                 0,0,3), nrow=3, ncol=3)
cov5 <- matrix(c(6,0,0,
                 0,5,0,
                 0,0,1), nrow=3, ncol=3)
sampN<-c(100,80,60,40,20)
covariance<-list(cov1,cov2,cov3,cov4,cov5)
set.seed(10)
y<-bain(estimate,"a=b=c=d=e",n=sampN,Sigma=covariance,group_parameters=1,joint_parameters = 2)
in1 <- solve(cov1/y$b[1])
in2 <- solve(cov2/y$b[2])
in3 <- solve(cov3/y$b[3])
in4 <- solve(cov4/y$b[4])
in5 <- solve(cov5/y$b[5])
invcov <- solve(y$prior)
invcov77 <- in1[3,3] + in2[3,3] + in3[3,3] + in4[3,3] + in5[3,3]
invcov67 <- in1[2,3] + in2[2,3] + in3[2,3] + in4[2,3] + in5[2,3]
invcov57 <- in5[1,3]
estimate<-c(0,0,0)
names(estimate) <- c("a", "b", "c")
sampN <- 100
covariance <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)
set.seed(10)
y<-bain(estimate,"a>0 & b>0 & c>0; a>b>c;a>b & b>0; a=b=c=0",n=sampN,Sigma=covariance,group_parameters=0,joint_parameters = 3)
set.seed(10)
z<-bain(estimate,"a=0;b=0;c=0",n=sampN,Sigma=covariance,group_parameters=0,joint_parameters = 3)

# test -------------------------------------------------------------------------
expect_equal(z$fit$Com[1:4], c(dnorm(0,0,5.77),dnorm(0,0,5.77),dnorm(0,0,5.77),NA), tolerance = .001)
