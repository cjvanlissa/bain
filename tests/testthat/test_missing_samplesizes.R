d <- sesamesim

d$age[10] <- NA
d$postnumb[3] <- NA
d$sex[1] <- NA
d$sex[2] <- NA
d$sex[239]<- NA
d$sex[240]<- NA
d$Ab[5]<- NA

# multiple regression

fit <- lm(postnumb ~ age, data = d)
res <- bain(fit, hypothesis = "age > .5")
test_that("bain default", {expect_equal(res$n,238)})

# anova

d$sex <- as.factor(d$sex)
fit <- lm(prenumb ~sex -1, data = d)
res <- bain(fit, hypothesis = "sex1 > sex2")
test_that("bain default", {expect_equal(res$n,c(113,123))})

# ancova

d$sex <- as.factor(d$sex)
fit <- lm(prenumb ~sex + age -1, data = d)
res <- bain(fit, hypothesis = "sex1 > sex2")
test_that("bain default", {expect_equal(res$n,c(112,123))})

# ttest independent groups
d$sex <- as.factor(d$sex)
fit <- t_test(prenumb ~sex, data = d)
res <- bain(fit, hypothesis = "group1 > group2")
test_that("bain default", {expect_equal(res$n,c(113,123))})

# lavaan single group

# use lavaan syntax to specify the confirmatory factor model
model1 <- '
A =~ Ab + Al + Af + An + Ar + Ac
B =~ Bb + Bl + Bf + Bn + Br + Bc
'

# use the lavaan sem function to execute the confirmatory factor analysis
fit1 <- sem(model1, data = d, std.lv = TRUE)

# formulate hypotheses, call bain, obtain summary stats


set.seed(100)
y <- bain(fit1, hypothesis = " A=~Ab > .6")
test_that("bain default", {expect_equal(y$n,239)})

# lavaan multiple group

# use lavaan syntax to specify the confirmatory factor model
model1 <- '
A =~ Ab + Al + Af + An + Ar + Ac
B =~ Bb + Bl + Bf + Bn + Br + Bc
'

# use the lavaan sem function to execute the confirmatory factor analysis
test_that("Grouping var with missing values returns warning", expect_warning(fit1 <- sem(model1, data = d, std.lv = TRUE, group = "sex")))

# formulate hypotheses, call bain, obtain summary stats

suppressWarnings(fit1 <- sem(model1, data = d, std.lv = TRUE, group = "sex"))
set.seed(100)
y <- bain(fit1, hypothesis = " A=~Ab.1 > .6")
test_that("bain default", {expect_equal(y$n,c(112,123))})
