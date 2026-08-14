# Extracted from test-extradefault.R:100

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "bain", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
data(sesamesim)
est1 <-c(1,2)
est2 <-c(3,4)
est3 <-c(-1,1)
estimate <- c(est1,est2,est3)
names(estimate) <- c("pre1", "post1","pre2", "post2","a1", "a2")
ngroup<-c(100,50)
cov1 <-matrix(c(1,0,0,0,
                0,1,0,0,
                0,0,1,0,
                0,0,0,1),nrow=4,ncol=4)
cov2 <- matrix(c(1,0,0,0,
                 0,1,0,0,
                 0,0,1,0,
                 0,0,0,1),nrow=4,ncol=4)
invtotcov <- matrix(c(1,0,0,0,0,0,
                      0,1,0,0,0,0,
                      0,0,1,0,0,0,
                      0,0,0,1,0,0,
                      0,0,0,0,2,0,
                      0,0,0,0,0,2),nrow=6,ncol=6)
totcov <- solve(invtotcov)
pricov1 <-matrix(c(200,0,0,0,
                   0,200,0,0,
                   0,0,200,0,
                   0,0,0,200),nrow=4,ncol=4)
pricov2 <- matrix(c(100,0,0,0,
                    0,100,0,0,
                    0,0,100,0,
                    0,0,0,100),nrow=4,ncol=4)
priinvtotcov <- matrix(c(.005,0,0,0,0,0,
                         0,.005,0,0,0,0,
                         0,0,.01,0,0,0,
                         0,0,0,.01,0,0,
                         0,0,0,0,.015,0,
                         0,0,0,0,0,.015),nrow=6,ncol=6)
pritotcov <- solve(priinvtotcov)
covariance<-list(cov1,cov2)
set.seed(100)
y <-bain(estimate, "pre1 - pre2 = post1 - post2;
               pre1 - pre2 > post1 - post2"  , n=ngroup, Sigma=covariance,
               group_parameters=2, joint_parameters = 2)
des2<-summary(y, ci = 0.95)
estimate <-c(1,2)
names(estimate) <- c("a1", "a2")
ngroup<-100
cov1 <-matrix(c(1,0,
                0,1),nrow=2,ncol=2)
covariance<-list(cov1)
set.seed(100)
y <-bain(estimate, "a1=a2"  , n=ngroup, Sigma=covariance,
         group_parameters=1, joint_parameters = 1)
des1<-summary(y, ci = 0.95)
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
        site2>(site1,site3,site4,site5)")

# test -------------------------------------------------------------------------
expect_equal(z$BFmatrix[2,8],z$fit$PMPb[2]/z$fit$PMPb[8])
