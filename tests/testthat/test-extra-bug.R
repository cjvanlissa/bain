# ==============================================================================
# TEST NUMBER 1: UNSTANDARDIZED AND STANDARDIZED
# ==============================================================================

# ===========================================================================
# BUG: STANDARDIZED = FALSE GEEFT DEZELFDE RESULTATEN ALS
# STANDARDIZED = TRUE. VOOR STD = FALSE WORDEN DE VERKEERDE
# PARAMETERS EN COV GEBRUIKT. HET MOETEN DE ONGESTANDAARDIZEERDE
# WORDEN
# ===========================================================================
data(sesamesim)

# read in the simulated sesamestreet data
sesamedata <- sesamesim

# use lavaan syntax to specify the confirmatory factor model
model1 <- 'Ab ~ Bb + Bl + 1'

# use the lavaan sem function to execute the confirmatory factor analysis
fit1 <- lavaan::sem(model1, data = sesamedata)

# HERE FOLLOWS THE CALL TO THE BAIN S3 FUNCTION WITH UNSTANDARDIZED PARAMETERS

hypotheses1 <-" Ab~Bb = Ab~Bl; Ab~Bb > Ab~Bl"
set.seed(100)
y1 <- bain(fit1,hypotheses1,standardize = FALSE)

# HERE FOLLOWS THE CALL TO BAIN DEFAULT WITH UNSTANDARDIZED PARAMETERS
estimate1 <- lavaan::coef(fit1)[1:2]
names(estimate1) <- c("a", "b")
covariance1 <- list(lavaan::vcov(fit1)[1:2,1:2])
ngroup1 <- lavaan::nobs(fit1)
hypotheses1 <-" a = b; a > b"
set.seed(100)
z1 <- bain(estimate1, hypotheses1, n =ngroup1, Sigma = covariance1,
          group_parameters = 2, joint_parameters = 0)

# TEST RESULTS UNSTANDARDIZED

test_that("Bain mutual", {expect_equal(y1$fit$Fit , z1$fit$Fit)})
test_that("Bain mutual", {expect_equal(y1$fit$Com , z1$fit$Com)})
test_that("Bain mutual", {expect_equal(y1$independent_restrictions, z1$independent_restrictions)})
test_that("Bain mutual", {expect_equal(y1$b, z1$b)})
test_that("Bain mutual", {expect_equal(as.vector(y1$posterior[1:2,1:2]), as.vector(z1$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(y1$prior[1:2,1:2]), as.vector(z1$prior))})
test_that("Bain mutual", {expect_equal(y1$fit$BF,z1$fit$BF)})
test_that("Bain mutual", {expect_equal(y1$fit$PMPb , z1$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(y1$BFmatrix)), as.vector(t(z1$BFmatrix)))})

# ==============================================================================
# TEST NUMBER 1a: UNSTANDARDIZED IS DE DEFAULT!
# ==============================================================================

hypotheses1 <-" Ab~Bb = Ab~Bl; Ab~Bb > Ab~Bl"
set.seed(100)
y1 <- bain(fit1,hypotheses1)

# TEST RESULTS UNSTANDARDIZED

test_that("Bain mutual", {expect_equal(y1$fit$Fit , z1$fit$Fit)})
test_that("Bain mutual", {expect_equal(y1$fit$Com , z1$fit$Com)})
test_that("Bain mutual", {expect_equal(y1$independent_restrictions, z1$independent_restrictions)})
test_that("Bain mutual", {expect_equal(y1$b, z1$b)})
test_that("Bain mutual", {expect_equal(as.vector(y1$posterior[1:2,1:2]), as.vector(z1$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(y1$prior[1:2,1:2]), as.vector(z1$prior))})
test_that("Bain mutual", {expect_equal(y1$fit$BF,z1$fit$BF)})
test_that("Bain mutual", {expect_equal(y1$fit$PMPb , z1$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(y1$BFmatrix)), as.vector(t(z1$BFmatrix)))})

# ==============================================================================
# TEST NUMBER 2: parameter labels instead of names with MULTIPLE GROUPS
# ==============================================================================

model1 <- 'age ~ peabody + 1'

sesamesim$sex <- factor(sesamesim$sex)

fit1 <- lavaan::sem(model1, data = sesamesim, group = "sex")
hypotheses1 <-"age~peabody.1 = age~peabody.2"
set.seed(100)
y1 <- bain(fit1,hypotheses1,standardize = TRUE)

model1 <- 'age ~ c(a1, a2)*peabody + c(b1, b2)*1'

sesamesim$sex <- factor(sesamesim$sex, labels = c("boy", "girl"))
fit1 <- lavaan::sem(model1, data = sesamesim, group = "sex")
hypotheses1 <-"a1 = a2"
set.seed(100)
y2 <- bain(fit1,hypotheses1,standardize = TRUE)

sesamesim$sex <- factor(sesamesim$sex, labels = c("boy", "girl"))
fit1 <- lavaan::sem(model1, data = sesamesim, group = "sex")
hypotheses1 <-"a1 = a2"
set.seed(100)
y3 <- bain(fit1,hypotheses1,standardize = TRUE)

test_that("Bain mutual", {expect_equal(y1$fit$BF,y2$fit$BF)})
test_that("Bain mutual", {expect_equal(y2$fit$BF,y3$fit$BF)})

# ==============================================================================
# TEST NUMBER 3: A GROWTH CURVE MODEL
# ==============================================================================

# ===================================================
# BUG: THE FIRST CALL TO BAIN BELOW GIVES AN ERROR MESSAGE
# ===================================================

model <- ' i =~ 1*Bb + 1*Bl + 1*Bf + 1*Bn + 1*Br + 1*Bc'

fit1 <- lavaan::growth(model, data=sesamesim)

# HERE FOLLOWS THE CALL TO THE BAIN S3 FUNCTION WITH STANDARDIZED PARAMETERS

hypotheses1 <-"i~1 = 3"
set.seed(100)
y2 <- bain(fit1,hypotheses1,standardize = TRUE)
#bain:::lav_get_estimates(fit1, standardize = TRUE)
# HERE FOLLOWS THE CALL TO BAIN DEFAULT WITH STANDARDIZED PARAMETERS
PE1 <- lavaan::parameterEstimates(fit1, standardize = TRUE)
estimate1 <- PE1[ PE1$op == "~1", "std.all"][7]
names(estimate1) <- c("i~1")
cov<- matrix(lavInspect(fit1, "vcov.std.all")["i~1", "i~1"],1,1)
covariance1 <- list(cov)
ngroup1 <- 240

z2 <- bain(estimate1, hypotheses1, n =ngroup1, Sigma = covariance1,
           group_parameters = 1, joint_parameters = 0)

# HERE FOLLOWS THE CHECK IF S3 GIVES THE SAME RESULTS AS DEFAULT

# TEST RESULTS

test_that("Bain mutual", {expect_equal(y2$fit$Fit , z2$fit$Fit)})
test_that("Bain mutual", {expect_equal(y2$fit$Com , z2$fit$Com)})
test_that("Bain mutual", {expect_equal(y2$independent_restrictions, z2$independent_restrictions)})
test_that("Bain mutual", {expect_equal(y2$b, z2$b)})
test_that("Bain mutual", {expect_equal(as.vector(y2$posterior), as.vector(z2$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(y2$prior), as.vector(z2$prior))})
test_that("Bain mutual", {expect_equal(y2$fit$BF,z2$fit$BF)})
test_that("Bain mutual", {expect_equal(y2$fit$PMPb , z2$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(y2$BFmatrix)), as.vector(t(z2$BFmatrix)))})

# ==============================================================================
# TEST NUMBER 4: A MULTIPLE GROUP MODEL WITH FIVE GROUPS
# ==============================================================================

data(sesamesim)
model1 <- 'age ~ peabody + 1'

fit1 <- lavaan::sem(model1, data = sesamesim, group = "site")
hypotheses1 <-"age~peabody.1 = age~peabody.2 = age~peabody.3 = age~peabody.4 = age~peabody.5"
set.seed(100)
y2 <- bain(fit1,hypotheses1,standardize = TRUE)

sesamesim$site <- factor(sesamesim$site, labels = c("a", "b", "c", "d", "e"))
fit1 <- lavaan::sem(model1, data = sesamesim, group = "site")
hypotheses1 <-"age~peabody.a = age~peabody.b = age~peabody.c = age~peabody.d = age~peabody.e"
set.seed(100)
y3 <- bain(fit1,hypotheses1,standardize = TRUE)

data(sesamesim)
sesamesim$site <- factor(sesamesim$site, labels = c("a", "b", "c", "d", "e"))
ngroup1 <- table(sesamesim$site)
PE3 <- lavaan::parameterEstimates(fit1, standardize = TRUE)
estimate1 <- PE3[ PE3$op == "~"  |PE3$op == "~1", "std.all"]
estimate1 <- estimate1[c(1,2,4,5,7,8,10,11,13,14)]
names(estimate1) <- do.call(paste0, data.frame(PE3[ PE3$op == "~"  |PE3$op == "~1", c("lhs", "op", "rhs")], ".g", PE3[ PE3$op == "~"  |PE3$op == "~1", ]$group))[c(1,2,4,5,7,8,10,11,13,14)]
names(estimate1) <- gsub("\\.g1", "", names(estimate1))

# obtain the covariance matrix of the standardized parameters for the boys
cov1 <- lavInspect(fit1, "vcov.std.all")[1:2,1:2]
cov2 <- lavInspect(fit1, "vcov.std.all")[4:5,4:5]
cov3 <- lavInspect(fit1, "vcov.std.all")[7:8,7:8]
cov4 <- lavInspect(fit1, "vcov.std.all")[10:11,10:11]
cov5 <- lavInspect(fit1, "vcov.std.all")[13:14,13:14]

# create a list of covariance matrices
covariance1 <- list(cov1,cov2,cov3,cov4,cov5)

# test the hypothesis of measurement invariance
# Hier ging het mis: er stond table(sesamesim$site), maar lavaan heeft de groepen niet op die volgorde staan
ngroup1 <- lavInspect(fit1, "nobs")
hypotheses1 <-"age~peabody = age~peabody.g2 = age~peabody.g3 = age~peabody.g4 = age~peabody.g5"

set.seed(100)
z2 <- bain(estimate1, hypotheses1, n = ngroup1, Sigma = covariance1,
           group_parameters = 2, joint_parameters = 0)
test_that("Bain mutual", {expect_equal(y2$n , z2$n)})
test_that("Bain mutual", {expect_equal(y2$fit$Fit , z2$fit$Fit)})
test_that("Bain mutual", {expect_equal(y2$fit$Com , z2$fit$Com)})
test_that("Bain mutual", {expect_equal(y2$independent_restrictions, z2$independent_restrictions)})
test_that("Bain mutual", {expect_equal(y2$b, z2$b)})
test_that("Bain mutual", {expect_equal(as.vector(y2$posterior), as.vector(z2$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(y2$prior), as.vector(z2$prior))})
test_that("Bain mutual", {expect_equal(y2$fit$BF,z2$fit$BF)})
test_that("Bain mutual", {expect_equal(y2$fit$PMPb , z2$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(y2$BFmatrix)), as.vector(t(z2$BFmatrix)))})

test_that("Bain mutual", {expect_equal(y3$n , z2$n)})
test_that("Bain mutual", {expect_equal(y3$fit$Fit , z2$fit$Fit)})
test_that("Bain mutual", {expect_equal(y3$fit$Com , z2$fit$Com)})
test_that("Bain mutual", {expect_equal(y3$independent_restrictions, z2$independent_restrictions)})
test_that("Bain mutual", {expect_equal(y3$b, z2$b)})
test_that("Bain mutual", {expect_equal(as.vector(y3$posterior), as.vector(z2$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(y3$prior), as.vector(z2$prior))})
test_that("Bain mutual", {expect_equal(y3$fit$BF,z2$fit$BF)})
test_that("Bain mutual", {expect_equal(y3$fit$PMPb , z2$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(y3$BFmatrix)), as.vector(t(z2$BFmatrix)))})

# ==============================================================================
# TEST NUMBER 5: A MULTIPLE GROUP MODEL WITH BETWEEN CONSTRAINTS
# ==============================================================================

model1 <- 'age ~ peabody + 1'

sesamesim$sex <- factor(sesamesim$sex)
fit1 <- lavaan::sem(model1, data = sesamesim, group = "sex",group.equal = c("intercepts"))
hypotheses1 <-"age~peabody.1 = age~peabody.2"
set.seed(100)
test_that("Multi-group models with between constraints fail", expect_error(y1 <- bain(fit1,hypotheses1,standardize = TRUE)))

# ==============================================================================
# TEST NUMBER 6: TEST THAT DEFINED PARAMETERS ARE DROPPED
# ==============================================================================

sesamedata <- sesamesim
model1 <- 'age ~ a*peabody + b*sex + 1
           def := a*b'
fit1 <- lavaan::sem(model1, data = sesamedata)
hypotheses1 <-"def = .4"
set.seed(100)
test_that("Defined pars are removed", expect_error(y1 <- bain(fit1,hypotheses1,standardized = TRUE)))

# ==============================================================================
# TEST NUMBER 7: TEST THAT MULTILEVEL MODELS DO NOT WORK
# ==============================================================================

model <- '
level: 1
fw =~ y1 + y2 + y3
fw ~ x1 + x2 + x3
level: 2
fb =~ y1 + y2 + y3
fb ~ w1 + w2
'
fit1 <- lavaan::sem(model, data = lavaan::Demo.twolevel, cluster = "cluster")
hypotheses1 <-"fw~x1=0"
set.seed(100)
test_that("Multilevel gives error", expect_error(y1 <- bain(fit1,hypotheses1,standardized = TRUE)))

# ==================================================
# TEST NUMBER 8: TWEEWEG ANOVA MET INTERACTIE EFFECT
# ==================================================
rm(sesamesim)
data(sesamesim)
data(sesamesim)
sesamesim$site <- as.factor(sesamesim$site)
sesamesim$sex <- as.factor(sesamesim$sex)

anov <- lm(postnumb~site*sex, data = sesamesim)
set.seed(100)
y1 <- bain(anov, "site2:sex2 < site4 < site4:sex2")

# HERE FOLLOWS THE CALL TO BAIN DEFAULT
estimate1 <- coef(anov)
names(estimate1) <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
covariance1 <- list(vcov(anov))
ngroup1 <- 240
hypotheses1 <-"g < d < i"
set.seed(100)
z1 <- bain(estimate1, hypotheses1, n =ngroup1, Sigma = covariance1,
           group_parameters = 10, joint_parameters = 0)

# TEST RESULTS

test_that("Bain mutual", {expect_equal(y1$fit$Fit , z1$fit$Fit)})
test_that("Bain mutual", {expect_equal(y1$fit$Com , z1$fit$Com)})
test_that("Bain mutual", {expect_equal(y1$independent_restrictions, z1$independent_restrictions)})
test_that("Bain mutual", {expect_equal(y1$b, z1$b)})
test_that("Bain mutual", {expect_equal(as.vector(y1$posterior[1]), as.vector(z1$posterior[67]))})
test_that("Bain mutual", {expect_equal(as.vector(y1$prior[1]), as.vector(z1$prior[67]))})
test_that("Bain mutual", {expect_equal(y1$fit$BF,z1$fit$BF)})
test_that("Bain mutual", {expect_equal(y1$fit$PMPb , z1$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(y1$BFmatrix)), as.vector(t(z1$BFmatrix)))})

# ==============================================================
# TEST NUMBER 8a: UNSTANDARDIZED REGRESSIE MET INTERACTIE EFFECT
# ==============================================================

data(sesamesim)
anov <- lm(postnumb~age*prenumb, data = sesamesim)
set.seed(100)
y1 <- bain(anov, "age:prenumb < 0")

# HERE FOLLOWS THE CALL TO BAIN DEFAULT
estimate1 <- coef(anov)
names(estimate1) <- c("a", "b", "c", "d")
covariance1 <- list(vcov(anov))
ngroup1 <- 240
hypotheses1 <-"d < 0"
set.seed(100)
z1 <- bain(estimate1, hypotheses1, n =ngroup1, Sigma = covariance1,
           group_parameters = 4, joint_parameters = 0)

# TEST RESULTS

test_that("Bain mutual", {expect_equal(y1$fit$Fit , z1$fit$Fit)})
test_that("Bain mutual", {expect_equal(y1$fit$Com , z1$fit$Com)})
test_that("Bain mutual", {expect_equal(y1$independent_restrictions, z1$independent_restrictions)})
test_that("Bain mutual", {expect_equal(y1$b, z1$b)})
test_that("Bain mutual", {expect_equal(as.vector(y1$posterior), as.vector(z1$posterior[4,4]))})
test_that("Bain mutual", {expect_equal(as.vector(y1$prior), as.vector(z1$prior[4,4]))})
test_that("Bain mutual", {expect_equal(y1$fit$BF,z1$fit$BF)})
test_that("Bain mutual", {expect_equal(y1$fit$PMPb , z1$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(y1$BFmatrix)), as.vector(t(z1$BFmatrix)))})

# =========================================================================
# TEST NUMBER 9: MULTIVARIATE USE OF LM
# =========================================================================

within <- lm(cbind(prenumb,postnumb,funumb)~sex*site, data=sesamesim)
set.seed(100)
test_that("Multivariate lm() gives error", expect_error(bain(within, "pre=post")))

# ==================================================================
# TEST NUMBER 10: FOUTMELDINGEN
# =============================================================================

set.seed(100)
sesamesim$site <- as.factor(sesamesim$site)
anov <- lm(postnumb~site-1,sesamesim)
est <- lavaan::coef(anov)
names(est)<-c("site1","site2","site3","site4","site5")
samp <- table(sesamesim$site)
var <- summary(anov)$sigma**2
cov1 <- var/samp[1]
cov2 <- var/samp[2]
cov3 <- var/samp[3]
cov4 <- var/samp[4]
cov5 <- var/samp[5]
cov1 <- matrix(cov1, nrow=1, ncol=1)
cov2 <- matrix(cov2, nrow=1, ncol=1)
cov3 <- matrix(cov3, nrow=1, ncol=1)
cov4 <- matrix(cov4, nrow=1, ncol=1)
cov5 <- matrix(cov5, nrow=1, ncol=1)
covmat <- list(cov1, cov2, cov3, cov4, cov5)

hyp1 <- "site1 = site2 & site1 > 0 & site2 > 0"
test_that("Nice error for incompatible hypotheses", expect_error(bain(anov, hyp1)))

hyp1 <- "site1 = site2 & site1 = 0 & site2 = 0"
test_that("Nice error for incompatible hypotheses", expect_error(bain(anov, hyp1)))

hyp1 <- "site1 = 0 & site1 > 0"
test_that("Nice error for incompatible hypotheses", expect_error(bain(anov, hyp1)))

est <- c(-1,0,1)
names(est) <- c("a", "b", "c")
samp <- 40
covariance <- matrix(c(1,1,0,
                       1,1,0,
                       0,0,1), nrow=3, ncol=3)
set.seed(100)
test_that("Nice error for incompatible hypotheses",
          expect_error(bain(est,"a = b = c",n=samp,
        Sigma=covariance,group_parameters=0,joint_parameters = 3)))



# ==================================================================
# TEST NUMBER 11: HYPOTHESES INVOEREN
#
# IS HET MOGELIJK DE HYPOTHESES OP HET VOLGENDE TE CHECKEN:
#
# ALS ER () GEBRUIKT WORDEN ZONDER DAT ER EEN , BINNEN DE ()
# STAAT MOET ER EEN FOUTMELDING KOMEN, HET IS EEN ABUSE VAN
# DE SYNTAX EN GEEFT NONSENSE ALS OUTPUT
# MET EEN TESTTHAT STATEMENT?
# ==================================================================

hyp <- "(site1 - site2) > 0"
test_that("Parentheses must contain commas", expect_error(bain(anov, hyp)))


