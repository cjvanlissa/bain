library(bain)
sesamesim$site <- as.factor(sesamesim$site)
anov <- lm(postnumb~site-1,sesamesim)

set.seed(100)
results <- bain(anov, "site1=site2=site3=site4=site5;
                       site2>site5>site1>site3>site4")
sensres <- bain_sensitivity(anov, "site1=site2=site3=site4=site5;
                       site2>site5>site1>site3>site4",
                       fractions = c(1,2,3,5))


test_that("Inequality constraints all the same", expect_true(length(unique(sapply(sensres, function(x){round(x$fit$BF[2], 3)}))) == 1))
test_that("Equality constraints not the same", expect_false(length(unique(sapply(sensres, function(x){round(x$fit$BF[1], 12)}))) == 1))

tmp <- summary(sensres, which_stat = "BF")
class(tmp)
test_that("summary.bain_sensitivity works", expect_s3_class(tmp, "sum_sensitivity"))
