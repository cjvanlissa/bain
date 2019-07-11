model2 <- '
    A  =~ Ab + Al + Af + An + Ar + Ac
    B =~ Bb + Bl + Bf + Bn + Br + Bc

    A ~ B + age + pea
'
fit2 <- sem(model2, data = sesameCFA, std.lv = TRUE)

# HERE FOLLOWS THE CALL TO THE BAIN S3 FUNCTION:

hypotheses2 <- "A~B > A~pea = A~age = 0;
               A~B > A~pea > A~age = 0;
A~B > A~pea > A~age > 0"

set.seed(100)
y1 <- bain(fit2, hypotheses2, fraction = 1, standardized = TRUE)

sy1 <- summary(y1, ci = 0.99)

set.seed(100)
y2 <- bain(fit2, hypotheses2, fraction = 2, standardized = TRUE)
set.seed(100)
y3 <- bain(fit2, hypotheses2, fraction = 3, standardized = TRUE)

# HERE FOLLOWS THE CALL TO BAIN DEFAULT

ngroup2 <- nobs(fit2)

PE2 <- parameterEstimates(fit2, standardized = TRUE)
# here, we only need the rows that correspond to regressions (ie op == "~"):
estimate2 <- PE2[ PE2$op == "~", "std.all"]

names(estimate2) <- c("before", "age", "pea")

PT2 <- parTable(fit2)
par.idx2 <- PT2$free[ PT2$op == "~" ]

covariance2 <- list(lavInspect(fit2, "vcov.std.all")[par.idx2, par.idx2])

hypotheses2 <- "before > pea = age = 0;
               before > pea > age = 0;
               before > pea > age > 0"

set.seed(100)
z1 <- bain(estimate2, hypotheses2, n = ngroup2, Sigma = covariance2,
                group_parameters = 3,joint_parameters = 0)
sz1<-summary(z1, ci = 0.99)

# Note that, below ngroup is divided by 2. This implies that the prior
# covariance matrix is based on twice Nmin.
set.seed(100)
z2 <- bain(estimate2, hypotheses2, n = ngroup2/2, Sigma = covariance2,
                 group_parameters = 3,joint_parameters = 0)

# Note that, below ngroup is divided by 3. This implies that the prior
# covariance matrix is based on three times Nmin.
set.seed(100)
z3 <- bain(estimate2, hypotheses2, n = ngroup2/3, Sigma = covariance2,
                 group_parameters = 3, joint_parameters = 0)

# HERE FOLLOWS THE CHECK IF S3 GIVES THE SAME RESULTS AS DEFAULT

# TEST RESULTS scale factor = 1

test_that("Bain mutual", {expect_equal(y1$fit$Fit , z1$fit$Fit)})
test_that("Bain mutual", {expect_equal(y1$fit$Com , z1$fit$Com)})
test_that("Bain mutual", {expect_equal(y1$independent_restrictions, z1$independent_restrictions)})
test_that("Bain mutual", {expect_equal(y1$b, z1$b)})
# Werkte niet omdat bain.lavaan alle nuissance parameters bevat.
# We houden voorlopig alle nuissance parameters in het output object.
test_that("Bain mutual", {expect_equal(as.vector(y1$posterior[13:15, 13:15]), as.vector(z1$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(y1$prior[13:15, 13:15]), as.vector(z1$prior))})
test_that("Bain mutual", {expect_equal(y1$fit$BF,z1$fit$BF)})
test_that("Bain mutual", {expect_equal(y1$fit$PMPb , z1$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(y1$BFmatrix)), as.vector(t(z1$BFmatrix)))})

# TEST SUMMARY scale factor = 1
# Werkt niet omdat bain.lavaan alle nuissance parameters bevat
test_that("summary", {expect_equal(sy1$Estimate[13:15] , sz1$Estimate)})
# Waarom zijn deze van verschillende lengte?
test_that("summary", {expect_equal(sy1$n[1] , sz1$n[1])})
test_that("summary", {expect_equal(sy1$lb[13:15] , sz1$lb)})
test_that("summary", {expect_equal(sy1$ub[13:15] , sz1$ub)})

# TEST RESULTS scale factor = 2

test_that("Bain mutual", {expect_equal(y2$fit$Fit , z2$fit$Fit)})
test_that("Bain mutual", {expect_equal(y2$fit$Com , z2$fit$Com)})
test_that("Bain mutual", {expect_equal(y2$independent_restrictions, z2$independent_restrictions)})
test_that("Bain mutual", {expect_equal(y2$b, z2$b)})
test_that("Bain mutual", {expect_equal(as.vector(y2$posterior[13:15, 13:15]), as.vector(z2$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(y2$prior[13:15, 13:15]), as.vector(z2$prior))})
test_that("Bain mutual", {expect_equal(y2$fit$BF,z2$fit$BF)})
test_that("Bain mutual", {expect_equal(y2$fit$PMPb , z2$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(y2$BFmatrix)), as.vector(t(z2$BFmatrix)))})

# TEST RESULTS scale factor = 3

test_that("Bain mutual", {expect_equal(y3$fit$Fit , z3$fit$Fit)})
test_that("Bain mutual", {expect_equal(y3$fit$Com , z3$fit$Com)})
test_that("Bain mutual", {expect_equal(y3$independent_restrictions, z3$independent_restrictions)})
test_that("Bain mutual", {expect_equal(y3$b, z3$b)})
test_that("Bain mutual", {expect_equal(as.vector(y3$posterior[13:15, 13:15]), as.vector(z3$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(y3$prior[13:15, 13:15]), as.vector(z3$prior))})
test_that("Bain mutual", {expect_equal(y3$fit$BF,z3$fit$BF)})
test_that("Bain mutual", {expect_equal(y3$fit$PMPb , z3$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(y3$BFmatrix)), as.vector(t(z3$BFmatrix)))})


