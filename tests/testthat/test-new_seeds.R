# Due to a change in CRAN policy, we are no longer allowed to generate random
# seeds in the way we previously did in bain.
# We have implemented a new way to generate seeds. This test checks whether those
# provide the same results as previously, and whether they provide consistent
# results upon repeat evaluation.
estimate <- c(1,1,1)
names(estimate)<-c("a", "b", "c")
sampN<- 100
cov <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)

set.seed(2)
y<-bain(estimate,"a>-.96 & a < 2.96 & b>-.96 & b < 2.96 & c>-.96 & c < 2.96",n=sampN,Sigma=cov,group_parameters=0,joint_parameters = 3)

test_that("New seed same result as old bain", {
  expect_equal(y$fit$Fit_in[1], 0.866728935817668)
  })

set.seed(2)
y2 <- bain(estimate,"a>-.96 & a < 2.96 & b>-.96 & b < 2.96 & c>-.96 & c < 2.96",n=sampN,Sigma=cov,group_parameters=0,joint_parameters = 3)

test_that("New seed same result as old bain", {
  expect_equal(y$fit$Fit_in[1], y2$fit$Fit_in[1])
})
