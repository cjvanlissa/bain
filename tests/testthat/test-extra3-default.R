# ======================================================================
# inequality constraints are evaluated two at a time
# below a check that this works correctly for even and uneven numbers
# ======================================================================

estimate <- c(0,0,0,0,0)
names(estimate)<-c("a", "b", "c","d", "e")
sampN<- 100
cov <- matrix(c(1,.0,.0,0,0,
                .0,1,.0,0,0,
                .0,.0,1,0,0,
                0,0,0,1,0,
                0,0,0,0,1),nrow=5,ncol=5)

set.seed(7)
y<-bain(estimate,"a>0 & b>0 & c>0 & d>0 & e>0",n=sampN,Sigma=cov,
        group_parameters=0,joint_parameters = 5)

test_that("bain default", {expect_equal(y$fit$Fit_in[1], .0312, tolerance = .001)})


estimate <- c(0,0,0,0)
names(estimate)<-c("a", "b", "c","d")
sampN<- 100
cov <- matrix(c(1,.0,.0,0,
                .0,1,.0,0,
                .0,.0,1,0,
                0,0,0,1),nrow=4,ncol=4)

set.seed(7)
y<-bain(estimate,"a>0 & b>0 & c>0 & d>0",n=sampN,Sigma=cov,
        group_parameters=0,joint_parameters = 4)
test_that("bain default", {expect_equal(y$fit$Fit_in[1], .0625)})

estimate <- c(0,0,0)
names(estimate)<-c("a", "b", "c")
sampN<- 100
cov <- matrix(c(1,.0,0,
                .0,1,0,
                0,0,1),nrow=3,ncol=3)

set.seed(7)
y<-bain(estimate,"a>0 & b>0 & c>0",n=sampN,Sigma=cov,group_parameters=0,
        joint_parameters = 3)
test_that("bain default", {expect_equal(y$fit$Fit_in[1], .125)})


estimate <- c(0,0)
names(estimate)<-c("a", "b")
sampN<- 100
cov <- matrix(c(1,.0,
                .0,1),nrow=2,ncol=2)

set.seed(7)
y<-bain(estimate,"a>0 & b>0",n=sampN,Sigma=cov,group_parameters=0,
        joint_parameters = 2)
test_that("bain default", {expect_equal(y$fit$Fit_in[1], .25)})

