# ===========================================================
# REGRESSION
# ===========================================================

# DEBUGGING THE EMPTy CONSTRAINTS SCREEN SITUATION
# THREE PREDICTORS - STANDARDIZE IS TRUE

sesamesim <- as.data.frame(cbind(sesamesim$postnumb,sesamesim$prenumb,sesamesim$funumb, sesamesim$peabody))
names(sesamesim)<-c("postnumb","prenumb","funumb","peabody")

bainResult <- bain:::bain_regression_cran(X=sesamesim,dep="postnumb",
                                   pred="prenumb funumb peabody",
                                   hyp=NULL,std = TRUE, seed=900)
set.seed(900)
tt <- lm(postnumb ~ prenumb + funumb + peabody, sesamesim)
ttout <-  bain(tt,"prenumb = 0 & funumb = 0 & peabody = 0",standardize=TRUE)

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
# THREE PREDICTORS - STANDARDIZE IS TRUE
bainResult <- bain:::bain_regression_cran(X=sesamesim,dep="postnumb",pred="prenumb funumb peabody",
                                   hyp="prenumb = funumb = peabody = 0;prenumb > 0 & funumb > 0 & peabody > 0 ",
                                   std = TRUE, seed=900)
set.seed(900)
tt <- lm(postnumb ~ prenumb + funumb + peabody, sesamesim)
ttout <-  bain(tt,"prenumb = funumb = peabody = 0;prenumb > 0 & funumb > 0 & peabody > 0 ",standardize = TRUE)

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

# ====================================================================

# DEBUGGING THE EMPTy CONSTRAINTS SCREEN SITUATION
# ONE PREDICTOR STANDARDIZE IS FALSE
library(bain)
sesamesim <- as.data.frame(cbind(sesamesim$postnumb,sesamesim$prenumb))
names(sesamesim)<-c("postnumb","prenumb")

bainResult <- bain:::bain_regression_cran(X=sesamesim,dep="postnumb",
                                   pred="prenumb",
                                   hyp=NULL,std = FALSE, seed=900)
set.seed(900)
tt <- lm(postnumb ~ prenumb, sesamesim)
ttout <-  bain(tt,"prenumb = 0",standardize=FALSE)

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
# ONE PREDICTOR STANDARDIZE IS FALSE
bainResult <- bain:::bain_regression_cran(X=sesamesim,dep="postnumb",
                                   pred="prenumb",
                                   hyp="prenumb = 0; prenumb > 0",
                                   std = FALSE, seed=900)
set.seed(900)
tt <- lm(postnumb ~ prenumb, sesamesim)
ttout <-  bain(tt,"prenumb = 0; prenumb > 0",standardize=FALSE)

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
