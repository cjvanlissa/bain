#===========================================================================
# THE T-TEST
# #===========================================================================



x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]
ttest <- t_test(x,y,paired = FALSE, var.equal = TRUE)
set.seed(100)
results <- bain(ttest, "x=y; x>y;x<y")
des1 <- summary(results, ci = 0.95)

test_that("summary", {expect_equal(des1$Estimate , as.numeric(results$estimates))})
test_that("summary", {expect_equal(des1$n , as.numeric(results$n))})
lbd <- as.vector(results$estimates + qnorm(.025) * c(sqrt(results$posterior[1,1]),sqrt(results$posterior[2,2])))
test_that("summary", {expect_equal(des1$lb , lbd)})
ubd <- as.vector(results$estimates + qnorm(.975) * c(sqrt(results$posterior[1,1]),sqrt(results$posterior[2,2])))
test_that("summary", {expect_equal(des1$ub , ubd)})

des2 <- summary(results, ci = 0.98)

test_that("summary", {expect_equal(des2$Estimate , as.numeric(results$estimates))})
test_that("summary", {expect_equal(des2$n , as.numeric(results$n))})
lbd <- as.vector(results$estimates + qnorm(.01) * c(sqrt(results$posterior[1,1]),sqrt(results$posterior[2,2])))
test_that("summary", {expect_equal(des2$lb , lbd)})
ubd <- as.vector(results$estimates + qnorm(.99) * c(sqrt(results$posterior[1,1]),sqrt(results$posterior[2,2])))
test_that("summary", {expect_equal(des2$ub , ubd)})

#===========================================================================
# THE ONE-SAMPLE T-TEST
#===========================================================================



ttest <- t_test(sesamesim$postnumb)
set.seed(100)
results <- bain(ttest, "x=30; x>30; x<30")
des1 <- summary(results, ci = 0.95)

test_that("summary", {expect_equal(des1$Estimate , as.numeric(results$estimates))})
test_that("summary", {expect_equal(des1$n , as.numeric(results$n))})
lbd <- as.vector(results$estimates + qnorm(.025) * c(sqrt(results$posterior)))
test_that("summary", {expect_equal(des1$lb , lbd)})
ubd <- as.vector(results$estimates + qnorm(.975) * c(sqrt(results$posterior)))
test_that("summary", {expect_equal(des1$ub , ubd)})

des2 <- summary(results, ci = 0.98)

test_that("summary", {expect_equal(des2$Estimate , as.numeric(results$estimates))})
test_that("summary", {expect_equal(des2$n , as.numeric(results$n))})
lbd <- as.vector(results$estimates + qnorm(.01) * c(sqrt(results$posterior)))
test_that("summary", {expect_equal(des2$lb , lbd)})
ubd <- as.vector(results$estimates + qnorm(.99) * c(sqrt(results$posterior)))
test_that("summary", {expect_equal(des2$ub , ubd)})

#===========================================================================
# ANOVA
#===========================================================================



sesamesim$site <- as.factor(sesamesim$site)
anov <- lm(postnumb~site-1,sesamesim)
set.seed(100)
results <- bain(anov, "site1=site2=site3=site4=site5; site2>site5>site1>site3>site4")
des1 <- summary(results, ci = 0.95)

test_that("summary", {expect_equal(des1$Estimate , as.numeric(results$estimates))})
test_that("summary", {expect_equal(des1$n , as.numeric(results$n))})
lbd <- as.vector(results$estimates + qnorm(.025) * c(sqrt(results$posterior[1,1]),sqrt(results$posterior[2,2]),sqrt(results$posterior[3,3]),
                                                                                sqrt(results$posterior[4,4]),sqrt(results$posterior[5,5])))
test_that("summary", {expect_equal(des1$lb , lbd)})
ubd <- as.vector(results$estimates + qnorm(.975) * c(sqrt(results$posterior[1,1]),sqrt(results$posterior[2,2]),sqrt(results$posterior[3,3]),
                                                     sqrt(results$posterior[4,4]),sqrt(results$posterior[5,5])))
test_that("summary", {expect_equal(des1$ub , ubd)})

des2 <- summary(results, ci = 0.98)

test_that("summary", {expect_equal(des2$Estimate , as.numeric(results$estimates))})
test_that("summary", {expect_equal(des2$n , as.numeric(results$n))})
lbd <- as.vector(results$estimates + qnorm(.01) * c(sqrt(results$posterior[1,1]),sqrt(results$posterior[2,2]),sqrt(results$posterior[3,3]),
                                                     sqrt(results$posterior[4,4]),sqrt(results$posterior[5,5])))
test_that("summary", {expect_equal(des2$lb , lbd)})
ubd <- as.vector(results$estimates + qnorm(.99) * c(sqrt(results$posterior[1,1]),sqrt(results$posterior[2,2]),sqrt(results$posterior[3,3]),
                                                     sqrt(results$posterior[4,4]),sqrt(results$posterior[5,5])))
test_that("summary", {expect_equal(des2$ub , ubd)})



#===========================================================================
# ANCOVA - BUG SUMMARY BREEKT
#===========================================================================



sesamesim$site <- as.factor(sesamesim$site)
sesamesim$prenumb <- sesamesim$prenumb - mean(sesamesim$prenumb)
ancov <- lm(postnumb~prenumb+site-1,sesamesim)
set.seed(100)
results <- bain(ancov, "site1=site2=site3=site4=site5; site2>site5>site1>site3>site4")
test_that("ANCOVA bain automatically moves covariate to the end", {expect_equal(names(results$estimates), c("site1", "site2", "site3", "site4", "site5", "prenumb"))})
des1 <- summary(results, ci = 0.95)

test_that("summary", {expect_equal(des1$Estimate , as.numeric(results$estimates))})
test_that("summary", {expect_equal(des1$n[1:5] , as.numeric(results$n)[1:5])})
lbd <- as.vector(results$estimates + qnorm(.025) * c(sqrt(results$posterior[1,1]),sqrt(results$posterior[2,2]),sqrt(results$posterior[3,3]),
                                                     sqrt(results$posterior[4,4]),sqrt(results$posterior[5,5]),sqrt(results$posterior[6,6])))
test_that("summary", {expect_equal(des1$lb , lbd)})
ubd <- as.vector(results$estimates + qnorm(.975) * c(sqrt(results$posterior[1,1]),sqrt(results$posterior[2,2]),sqrt(results$posterior[3,3]),
                                                     sqrt(results$posterior[4,4]),sqrt(results$posterior[5,5]),sqrt(results$posterior[6,6])))
test_that("summary", {expect_equal(des1$ub , ubd)})

des2 <- summary(results, ci = 0.98)

test_that("summary", {expect_equal(des2$Estimate , as.numeric(results$estimates))})
test_that("summary", {expect_equal(des2$n[1:5], as.numeric(results$n)[1:5])})
lbd <- as.vector(results$estimates + qnorm(.01) * c(sqrt(results$posterior[1,1]),sqrt(results$posterior[2,2]),sqrt(results$posterior[3,3]),
                                                     sqrt(results$posterior[4,4]),sqrt(results$posterior[5,5]),sqrt(results$posterior[6,6])))
test_that("summary", {expect_equal(des2$lb , lbd)})
ubd <- as.vector(results$estimates + qnorm(.99) * c(sqrt(results$posterior[1,1]),sqrt(results$posterior[2,2]),sqrt(results$posterior[3,3]),
                                                     sqrt(results$posterior[4,4]),sqrt(results$posterior[5,5]),sqrt(results$posterior[6,6])))
test_that("summary", {expect_equal(des2$ub , ubd)})

#===========================================================================
# MULTIPLE REGRESSION
#===========================================================================



regr <- lm(postnumb ~ age + peabody + prenumb,sesamesim)
set.seed(100)
results<-bain(regr, "age = 0 & peab=0 & pre=0 ; age > 0 & peab > 0 & pre > 0 ", standardize = FALSE)
des1 <- summary(results, ci = 0.95)

test_that("summary", {expect_equal(des1$Estimate , as.numeric(results$estimates))})
test_that("summary", {expect_equal(des1$n , c(240,240,240))})
lbd <- as.vector(results$estimates + qnorm(.025) * c(sqrt(results$posterior[1,1]),sqrt(results$posterior[2,2]),sqrt(results$posterior[3,3])))
test_that("summary", {expect_equal(des1$lb , lbd)})
ubd <- as.vector(results$estimates + qnorm(.975) * c(sqrt(results$posterior[1,1]),sqrt(results$posterior[2,2]),sqrt(results$posterior[3,3])))
test_that("summary", {expect_equal(des1$ub , ubd)})

des2 <- summary(results, ci = 0.98)

test_that("summary", {expect_equal(des2$Estimate , as.numeric(results$estimates))})
test_that("summary", {expect_equal(des2$n , c(240,240,240))})
lbd <- as.vector(results$estimates + qnorm(.01) * c(sqrt(results$posterior[1,1]),sqrt(results$posterior[2,2]),sqrt(results$posterior[3,3])))
test_that("summary", {expect_equal(des2$lb , lbd)})
ubd <- as.vector(results$estimates + qnorm(.99) * c(sqrt(results$posterior[1,1]),sqrt(results$posterior[2,2]),sqrt(results$posterior[3,3])))
test_that("summary", {expect_equal(des2$ub , ubd)})


# REPEATED MEASUERES WITH ONE WITHIN FACTOR ONLY


within <- lm(cbind(prenumb,postnumb,funumb)~1, data=sesamesim)
estimate <- coef(within)[1:3]
names(estimate) <- c("pre", "post", "fu")
ngroup <- nrow(sesamesim)
covmatr <- list(vcov(within))
set.seed(100)
results <- bain(estimate,"pre = post = fu; pre < post < fu", n=ngroup,
                Sigma=covmatr, group_parameters=3, joint_parameters = 0)
des2<-summary(results, ci = 0.95)
test_that("summary", {expect_equal(des2$n , c(240,240,240))})

# REPEATED MEASURES WITH A WITHIN AND BETWEEN FACTOR


sesamesim$sex <- factor(sesamesim$sex)
bw <- lm(cbind(prenumb, postnumb, funumb)~sex-1, data=sesamesim)
coef(bw)
est1 <-coef(bw)[1,1:3] # the three means for sex = 1
est2 <-coef(bw)[2,1:3] # the three means for sex = 2
estimate <- c(est1,est2)
names(estimate) <- c("pre1", "post1", "fu1","pre2", "post2", "fu2")
ngroup<-table(sesamesim$sex)
cov1 <- c(vcov(bw)[1,1],vcov(bw)[1,3],vcov(bw)[1,5],vcov(bw)[3,1],
          vcov(bw)[3,3],vcov(bw)[3,5],vcov(bw)[5,1],vcov(bw)[5,3],vcov(bw)[5,5])
cov1 <- matrix(cov1,3,3)
cov2 <- c(vcov(bw)[2,2],vcov(bw)[2,4],vcov(bw)[2,6],vcov(bw)[4,2],
          vcov(bw)[4,4],vcov(bw)[4,6],vcov(bw)[6,2],vcov(bw)[6,4],vcov(bw)[6,6])
cov2 <- matrix(cov2,3,3)
covariance<-list(cov1,cov2)
set.seed(100)
results <-bain(estimate, "pre1 - pre2 = post1 - post2 = fu1 -fu2;
               pre1 - pre2 > post1 - post2 > fu1 -fu2"  , n=ngroup, Sigma=covariance,
               group_parameters=3, joint_parameters = 0)
des2<-summary(results, ci = 0.95)
test_that("summary", {expect_equal(des2$n , c(115,115,115,125,125,125))})

