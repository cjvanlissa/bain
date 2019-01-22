sesamesim$sex <- factor(sesamesim$sex)
bw <- lm(cbind(prenumb, postnumb, funumb)~sex-1 + age + peabody, data=sesamesim)
coef(bw)
est1 <-coef(bw)[1,1:3] # the three means for sex = 1
est2 <-coef(bw)[2,1:3] # the three means for sex = 2
est3 <-coef(bw)[3,1:3] # the covs for age
est4 <-coef(bw)[4,1:3] # the covs for peabody
estimate <- c(est1,est2,est3,est4)
names(estimate) <- c("pre1", "post1", "fu1","pre2", "post2", "fu2","a1", "a2", "a3", "p1", "p2", "p3")
ngroup<-table(sesamesim$sex)

cov1 <-vcov(bw)
cov2 <- vcov(bw)
covariance<-list(cov1,cov2)
set.seed(100)
test_that("Bain stops MANOVA with covariates", expect_error(bain(estimate, "pre1 - pre2 = post1 - post2 = fu1 -fu2;
               pre1 - pre2 > post1 - post2 > fu1 -fu2"  , n=ngroup, Sigma=covariance,
                                                                 group_parameters=3, joint_parameters = 6)))

