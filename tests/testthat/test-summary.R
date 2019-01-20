#===========================================================================
# THE T-TEST
# #===========================================================================

rm(list=ls())

x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]
ttest <- t.test(x,y,paired = FALSE, var.equal = TRUE)
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

rm(list=ls())

ttest <- t.test(sesamesim$postnumb)
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

rm(list=ls())

sesamesim$site <- as.factor(sesamesim$site)
anov <- lm(postnumb~site-1,sesamesim)
set.seed(100)
results <- bain(anov, "site1=site2=site3=site4=site5; site2>site5>site1>site3>site4")
des1 <- summary(results, ci = 0.95)

test_that("summary", {expect_equal(des1$Estimate , as.numeric(results$estimates))})
test_that("summary", {expect_equal(des1$n.Freq , as.numeric(results$n))})
lbd <- as.vector(results$estimates + qnorm(.025) * c(sqrt(results$posterior[1,1]),sqrt(results$posterior[2,2]),sqrt(results$posterior[3,3]),
                                                                                sqrt(results$posterior[4,4]),sqrt(results$posterior[5,5])))
test_that("summary", {expect_equal(des1$lb , lbd)})
ubd <- as.vector(results$estimates + qnorm(.975) * c(sqrt(results$posterior[1,1]),sqrt(results$posterior[2,2]),sqrt(results$posterior[3,3]),
                                                     sqrt(results$posterior[4,4]),sqrt(results$posterior[5,5])))
test_that("summary", {expect_equal(des1$ub , ubd)})

des2 <- summary(results, ci = 0.98)

test_that("summary", {expect_equal(des2$Estimate , as.numeric(results$estimates))})
test_that("summary", {expect_equal(des2$n.Freq , as.numeric(results$n))})
lbd <- as.vector(results$estimates + qnorm(.01) * c(sqrt(results$posterior[1,1]),sqrt(results$posterior[2,2]),sqrt(results$posterior[3,3]),
                                                     sqrt(results$posterior[4,4]),sqrt(results$posterior[5,5])))
test_that("summary", {expect_equal(des2$lb , lbd)})
ubd <- as.vector(results$estimates + qnorm(.99) * c(sqrt(results$posterior[1,1]),sqrt(results$posterior[2,2]),sqrt(results$posterior[3,3]),
                                                     sqrt(results$posterior[4,4]),sqrt(results$posterior[5,5])))
test_that("summary", {expect_equal(des2$ub , ubd)})



#===========================================================================
# ANCOVA - BUG SUMMARY BREEKT
#===========================================================================

rm(list=ls())

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

rm(list=ls())

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
