data(sesamesim)
sesameCFA <- sesamesim
names(sesameCFA)[6] <- "pea"
model1 <- '
    A =~ Ab + para*Al + parb*Af + An + Ar + Ac
B =~ Bb + Bl + Bf + Bn + Br + Bc
A~~B
calculatedpar := para+parb'

# use the lavaan sem function to execute the confirmatory factor analysis
fit1 <- sem(model1, data = sesameCFA, std.lv = TRUE)
partable(fit1)


set.seed(100)
test_that("Can use parameter labels", expect_error({y <- bain(fit1,"para > parb",standardized = TRUE)}, NA))
keep_y <- bain(fit1,"para > parb",standardized = TRUE)
test_that("Parameter labels yield correct result", expect_equal(keep_y$fit$PMPb[1], .268, tolerance = .001))

test_that("only factor loadings retained", expect_equal(length(keep_y$estimates), 12))
test_that("all covariances dropped", expect_true(all(!grepl("~~", names(keep_y$estimates)))))
test_that("calculatedpar dropped", expect_true(all(!grepl("calculatedpar", names(keep_y$estimates)))))
