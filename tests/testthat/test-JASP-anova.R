# ===========================================================
# ANOVA
# ===========================================================

# DEBUGGING THE EMPT CONSTRAINTS SCREEN SITUATION
data(sesamesim)
sesamesim <- as.data.frame(cbind(sesamesim$postnumb,sesamesim$site))
names(sesamesim)<-c("postnumb","site")

bainResult <- bain:::bain_anova_cran(X=sesamesim,dep="postnumb",
              group="site",hyp=NULL,seed=900)
set.seed(900)
sesamesim$site <- as.factor(sesamesim$site)
tt <- lm(postnumb ~ site -1, sesamesim)
ttout <-  bain(tt,"site1 = site2 = site3 = site4 = site5")

# COMPARING THE WRAPPER FOR JASP WITH BAIN
test_that("Bain mutual", {expect_equal(bainResult$fit$Fit , ttout$fit$Fit)})
test_that("Bain mutual", {expect_equal(bainResult$fit$Com , ttout$fit$Com)})
test_that("Bain mutual", {expect_equal(bainResult$b, ttout$b)})
test_that("Bain mutual", {expect_equal(as.vector(bainResult$posterior), as.vector(ttout$posterior))})
test_that("Bain mutual", {expect_equal(bainResult$fit$BF,ttout$fit$BF)})
test_that("Bain mutual", {expect_equal(bainResult$fit$PMPb , ttout$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(bainResult$BFmatrix)), as.vector(t(ttout$BFmatrix)))})
test_that("Bain mutual", {expect_equal(summary(bainResult),summary(ttout))})

# # COMPARING JASP WITH THE WRAPPER FOR JASP
# print(bainResult)
# print(bainResult$BFmatrix)
# print(summary(bainResult))

# DEBUGGING THE FILLED IN CONSTRAINTS SCREEN SITUATION

bainResult <- bain:::bain_anova_cran(X=sesamesim,dep="postnumb",
              group="site",hyp="site1 =site2=site3=site4=site5;
              site3 < site4 < site1 < site5 < site2",seed=900)
set.seed(900)
sesamesim$site <- as.factor(sesamesim$site)
tt <- lm(postnumb ~ site -1, sesamesim)
ttout <-  bain(tt,"site1 =site2=site3=site4=site5;
              site3 < site4 < site1 < site5 < site2")

# COMPARING THE WRAPPER FOR JASP WITH BAIN
test_that("Bain mutual", {expect_equal(bainResult$fit$Fit , ttout$fit$Fit)})
test_that("Bain mutual", {expect_equal(bainResult$fit$Com , ttout$fit$Com, tolerance = .0001)})
test_that("Bain mutual", {expect_equal(bainResult$b, ttout$b)})
test_that("Bain mutual", {expect_equal(as.vector(bainResult$posterior), as.vector(ttout$posterior))})
test_that("Bain mutual", {expect_equal(round(bainResult$fit$BF),round(ttout$fit$BF))})
test_that("Bain mutual", {expect_equal(bainResult$fit$PMPb , ttout$fit$PMPb, tolerance = .0001)})
test_that("Bain mutual", {expect_equal(as.vector(t(bainResult$BFmatrix)), as.vector(t(ttout$BFmatrix)))})
test_that("Bain mutual", {expect_equal(summary(bainResult),summary(ttout))})

# # COMPARING JASP WITH THE WRAPPER FOR JASP
# print(bainResult)
# print(bainResult$BFmatrix)
# print(summary(bainResult))
