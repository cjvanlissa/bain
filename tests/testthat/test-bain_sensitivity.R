data(sesamesim)
sesamesim$site <- as.factor(sesamesim$site)
anov <- lm(postnumb~site-1,sesamesim)

set.seed(100)
sensres <- bain_sensitivity(anov, "site1=site2=site3=site4=site5;
                       site2>site5>site1>site3>site4",
                       fractions = c(1,2,3,5))

test_that("Inequality constraints all the same", expect_true(length(unique(sapply(sensres, function(x){round(x$fit$BF[2], 3)}))) == 1))
test_that("Equality constraints not the same", expect_false(length(unique(sapply(sensres, function(x){round(x$fit$BF[1], 12)}))) == 1))


tmp <- summary(sensres, which_stat = "BF")
test_that("summary.bain_sensitivity works", expect_s3_class(tmp, "sum_sensitivity"))

set.seed(100)
sensres <- bain_sensitivity(anov, "site3=site4;
                            site3 < site4",
                            fractions = c(1,2,3,5))

test_that("Inequality constraints all the same", expect_true(length(unique(sapply(sensres, function(x){round(x$fit$BF[2], 3)}))) == 1))
test_that("Equality constraints not the same", expect_false(length(unique(sapply(sensres, function(x){round(x$fit$BF[1], 12)}))) == 1))

set.seed(100)
ana1 <- bain(anov, "site3=site4;
                            site3 < site4",
             fraction = 1)

test_that("correcte BFs", expect_equal(summary(sensres)[1,2], ana1$fit$BF[1]))
test_that("correcte BFs", expect_equal(summary(sensres)[1,3],ana1$fit$BF[2]))


set.seed(100)
ana3 <- bain(anov, "site3=site4;
                            site3 < site4",
             fraction = 3)

test_that("correcte BFs", expect_equal(summary(sensres)[3,2],ana3$fit$BF[1]))
test_that("correcte BFs", expect_equal(summary(sensres)[3,3],ana3$fit$BF[2]))

# tests Herbert after adding PMPc to the main output table

set.seed(100)
sensres <- bain_sensitivity(anov, "site3=site4;site3 < site4",fractions = c(1,2,3,5))
set.seed(100)
ana1 <- bain(anov, "site3=site4;site3 < site4",fraction = 1)
set.seed(100)
ana3 <- bain(anov, "site3=site4;site3 < site4",fraction = 3)
tmp1 <- summary(sensres, which_stat = "PMPc")
tmp2 <- summary(sensres, which_stat = "PMPb")
tmp3 <- summary(sensres, which_stat = "PMPa")
tmp4 <- summary(sensres, which_stat = "BF.u")

test_that("check", expect_equal(c(summary(sensres)[1,2],summary(sensres)[3,2]),
                                c(ana1$fit$BF[1],ana3$fit$BF[1])   ))
test_that("check", expect_equal(c(ana1$fit$PMPc[1],ana1$fit$PMPc[4],ana3$fit$PMPc[1],ana3$fit$PMPc[4]), 
                                c(tmp1[1,2],tmp1[1,4],tmp1[3,2],tmp1[3,4])   ))
test_that("check", expect_equal(c(ana1$fit$PMPb[1],ana1$fit$PMPb[3],ana3$fit$PMPb[1],ana3$fit$PMPb[3]), 
                                c(tmp2[1,2],tmp2[1,4],tmp2[3,2],tmp2[3,4])   ))


set.seed(100)
sensres <- bain_sensitivity(anov, "site3=site4",fractions = c(1,2,3,5))
set.seed(100)
ana1 <- bain(anov, "site3=site4",fraction = 1)
set.seed(100)
ana3 <- bain(anov, "site3=site4",fraction = 3)
tmp1 <- summary(sensres, which_stat = "PMPc")
tmp2 <- summary(sensres, which_stat = "PMPb")
tmp3 <- summary(sensres, which_stat = "PMPa")
tmp4 <- summary(sensres, which_stat = "BF.u")

test_that("check", expect_equal(c(summary(sensres)[1,2],summary(sensres)[3,2]),
                                c(ana1$fit$BF[1],ana3$fit$BF[1])   ))
test_that("check", expect_equal(c(ana1$fit$PMPc[1],ana1$fit$PMPc[3],ana3$fit$PMPc[1],ana3$fit$PMPc[3]), 
                                c(tmp1[1,2],tmp1[1,3],tmp1[3,2],tmp1[3,3])   ))
test_that("check", expect_equal(c(ana1$fit$PMPb[1],ana1$fit$PMPb[2],ana3$fit$PMPb[1],ana3$fit$PMPb[2]), 
                                c(tmp2[1,2],tmp2[1,3],tmp2[3,2],tmp2[3,3])   ))



