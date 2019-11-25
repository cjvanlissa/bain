# TESTING COMBINATION OF = AND ><

estimate <- c(0,0,0)
names(estimate)<-c("a", "b", "c")
sampN<- 100
cov <- matrix(c(1,0,0,
                0,1,0,
                0,0,1),nrow=3,ncol=3)

set.seed(7)
y<-bain(estimate,"a>0 & b>0 & c=0",n=sampN,Sigma=cov,group_parameters=0,
        joint_parameters = 3)

test_that("bain default", {expect_equal(y$fit$Fit_in[1], .25)})
test_that("bain default", {expect_equal(y$fit$Com_in[1], .25)})
test_that("bain default", {expect_equal(y$fit$Fit_eq[1], .399, tolerance=.001)})



# =============================================================
estimate <- c(0,0)
names(estimate)<-c("a", "b")
sampN<- 100
cov <- matrix(c(1,.5,
                .5,1),nrow=2,ncol=2)

set.seed(7)
y<-bain(estimate,"a=10 & b>0",n=sampN,Sigma=cov,group_parameters=0,
        joint_parameters = 2)

test_that("bain default", {expect_equal(y$fit$Fit_in[1], 1.0)})
test_that("bain default", {expect_equal(y$fit$Com_in[1], 0.5)})





