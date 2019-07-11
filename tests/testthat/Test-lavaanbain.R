# TESTING

# Latent Regression

# Specify a latent regression model
model <- 'visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
speed ~ text*textual +vis*visual'

# Estimate the parameters of the latent regression model with lavaan
fit<-sem(model,data=HolzingerSwineford1939,std.lv = TRUE)

# determine the sample size
ngroup<-nobs(fit)

estimate<-standardizedSolution(fit)[10:11,4]
# assign names to the estimates
names(estimate) <- c("textual","visual")

covariance<-list(lavInspect(fit, "vcov.std.all")[10:11,10:11])

# set a seed value
set.seed(100)
# test hypotheses with bain (manual input)
z <- bain(estimate,"visual=textual=0; visual > textual >
                0",n=ngroup,Sigma=covariance,group_parameters=2,joint_parameters = 0)

# using bain.lavaan
set.seed(100)
lavres <- bain(fit, "vis=text=0; vis > text >0")


test_that("Bain mutual", {expect_equal(lavres$fit$Fit , z$fit$Fit)})
test_that("Bain mutual", {expect_equal(lavres$fit$Com , z$fit$Com)})
test_that("Bain mutual", {expect_equal(lavres$independent_restrictions, z$independent_restrictions)})
test_that("Bain mutual", {expect_equal(lavres$b, z$b)})
test_that("Bain mutual", {expect_equal(as.vector(lavres$posterior), as.vector(z$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(lavres$prior), as.vector(z$prior))})
test_that("Bain mutual", {expect_equal(lavres$fit$BF,z$fit$BF)})
test_that("Bain mutual", {expect_equal(lavres$fit$PMPb , z$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(lavres$BFmatrix)), as.vector(t(z$BFmatrix)))})

# Only difference in that the lav_in_bain uses all parameters (differences in lenght of posterior)
# NO DIFFERENCE in fit, complement, BF...

## Regression by lm and regression by sem


# regression by lm

regr <- lm(postnumb ~ age + peabody + prenumb,sesamesim)

set.seed(100)
z<-bain(regr, "0<age<peab<pre "
        , standardize = T)


# regression via LavBain
library(lavaan)
regresmod <- "postnumb ~ ag* age+ peab*peabody+ pre*prenumb
"

regrfit            <- sem(model = regresmod,data = sesamesim, fixed.x=F)

set.seed(100)
ylav <- bain(regrfit, standardize = T,
                    hypothesis = "0<ag<peab<pre ")


# TESTING bain.lavaan AND bain.lm against eachother

test_that("Bain mutual", {expect_equal(ylav$fit$Fit , z$fit$Fit)})
test_that("Bain mutual", {expect_equal(ylav$fit$Com , z$fit$Com)})
test_that("Bain mutual", {expect_equal(ylav$independent_restrictions, z$independent_restrictions)})
test_that("Bain mutual", {expect_equal(ylav$b, z$b)})
test_that("Bain mutual", {expect_equal(as.vector(ylav$posterior), as.vector(z$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(ylav$prior), as.vector(z$prior))})
test_that("Bain mutual", {expect_equal(ylav$fit$BF,z$fit$BF)})
test_that("Bain mutual", {expect_equal(ylav$fit$PMPb , z$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(ylav$BFmatrix)), as.vector(t(z$BFmatrix)))})

# Some minor differences in BF.


## Multiple group CFA.


# Specify a latent regression model
model <- 'visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
speed ~ c(pas.text, grant.text)*textual + c(pas.vis, grant.vis)*visual'

# Estimate the parameters of the latent regression model with lavaan
fit<-sem(model,data=HolzingerSwineford1939, group = "school", std.lv = TRUE)

# determine the sample size
ngroup<-nobs(fit)

estimate<-standardizedSolution(fit)[c(10,11,46,47),5]
# assign names to the estimates
names(estimate) <- c("textual.Pasteur","visual.Pasteur","textual.GrantWhite","visual.GrantWhite" )

covariance<-list(lavInspect(fit, "vcov.std.all")[c(10,11,40,41),c(10,11,40,41)])
covariance<-list(lavInspect(fit, "vcov.std.all")[c(10,11),c(10,11)],lavInspect(fit, "vcov.std.all")[c(40,41),c(40,41)])
# set a seed value
set.seed(100)
# test hypotheses with bain
z <- bain(estimate,"visual.GrantWhite=textual.GrantWhite=0; visual.GrantWhite > visual.Pasteur >0",
          n=lavInspect(fit, "Nobs") ,Sigma=covariance,group_parameters=2,joint_parameters = 0)

# Done using bain.lavaan
set.seed(100)
lavres <- bain(x = fit,standardize = T,hypothesis = "grant.vis=grant.text=0; grant.vis > pas.vis >0")

z$estimates

test_that("Bain mutual", {expect_equal(lavres$fit$Fit , z$fit$Fit)})
test_that("Bain mutual", {expect_equal(lavres$fit$Com , z$fit$Com)})
test_that("Bain mutual", {expect_equal(lavres$independent_restrictions, z$independent_restrictions)})
test_that("Bain mutual", {expect_equal(lavres$b, z$b)})
test_that("Bain mutual", {expect_equal(as.vector(lavres$posterior), as.vector(z$posterior))})
test_that("Bain mutual", {expect_equal(as.vector(lavres$prior), as.vector(z$prior))})
test_that("Bain mutual", {expect_equal(lavres$fit$BF,z$fit$BF)})
test_that("Bain mutual", {expect_equal(lavres$fit$PMPb , z$fit$PMPb)})
test_that("Bain mutual", {expect_equal(as.vector(t(lavres$BFmatrix)), as.vector(t(z$BFmatrix)))})

# Only difference is in length of the posterior and prior matrices, due to many more parameters included with my method.


# Multiple groups with constrained parameters -----------------------------

# Specify a latent regression model
model <- 'visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ c(a, a)*x7 + x8 + x9
speed ~ c(pas.text, grant.text)*textual + c(pas.vis, grant.vis)*visual'

# Estimate the parameters of the latent regression model with lavaan
fit<-sem(model,data=HolzingerSwineford1939, group = "school", std.lv = TRUE)

bain(fit, "pas.text > grant.text")



# Specify a latent regression model
model <- 'visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ c(a, a)*x7 + x8 + x9
speed ~ c(pas.text, grant.text)*textual + c(pas.vis, grant.vis)*visual'

# Estimate the parameters of the latent regression model with lavaan
fit<-sem(model,data=HolzingerSwineford1939, group = "school", std.lv = TRUE)

bain(fit, "pas.text > grant.text")

