#====================================================
# test hypo lavaan mbt equated parameters moet 
# foutmelding geven
#====================================================

# multiple group 1 equality constraints - constraints on restricted pars
model1 <- 'postnumb ~ c(v2,v3)*age + c(v2,v3)*peabody'
sesamesim$sex <- factor(sesamesim$sex, labels = c("boy", "girl"))
fit1 <- sem(model1, data = sesamesim, std.lv = TRUE, group = "sex")
hypotheses1 <- "v2 > v3"
set.seed(100)
#y1 <- bain(fit3, hypotheses31, standardize = TRUE)
test_that("equal constraints return error", expect_error(y1 <- bain(fit1,hypotheses1,standardize = TRUE)))


# multiple group 1 equality constraints - constraints on unrestricted pars
model2 <- 'postnumb ~ c(v2,v3)*age + c(v2,v3)*peabody + prenumb'
sesamesim$sex <- factor(sesamesim$sex, labels = c("boy", "girl"))
fit2 <- sem(model2, data = sesamesim, std.lv = TRUE, group = "sex")
hypotheses2 <- "postnumb~prenumb > 0"
set.seed(100)
test_that("equal constraints return error", expect_error(y2 <- bain(fit2,hypotheses2,standardize = TRUE)))

# multiple group 2 equality constraints - constraints on restricted pars
model3 <- 'postnumb ~ c(v2,v3)*age + c(v2,v3)*peabody + c(v4,v5)*prenumb + c(v4,v5)*funumb'
sesamesim$sex <- factor(sesamesim$sex, labels = c("boy", "girl"))
fit3 <- sem(model3, data = sesamesim, std.lv = TRUE, group = "sex")
hypotheses3 <- "v2 > 0"
set.seed(100)
test_that("equal constraints return error", expect_error(y3 <- bain(fit3,hypotheses3,standardize = TRUE)))

# multiple group 2 equality constraints - constraints on unrestricted pars
model4 <- 'postnumb ~ c(v2,v3)*age + c(v2,v3)*peabody + c(v4,v5)*prenumb + c(v4,v5)*funumb + viewenc'
sesamesim$sex <- factor(sesamesim$sex, labels = c("boy", "girl"))
fit4 <- sem(model4, data = sesamesim, std.lv = TRUE, group = "sex")
hypotheses4 <- "postnumb~viewenc > 0"
set.seed(100)
test_that("equal constraints return error", expect_error(y4 <- bain(fit4,hypotheses4,standardize = TRUE)))

# standardize is true met between group constraints
model5 <- 'postnumb ~ c(v2,v2)*age + peabody + prenumb + funumb + viewenc'
sesamesim$sex <- factor(sesamesim$sex, labels = c("boy", "girl"))
fit5 <- sem(model5, data = sesamesim, std.lv = TRUE, group = "sex")
hypotheses5 <- "postnumb~viewenc.girl > 0"
set.seed(100)
test_that("equal constraints return error", expect_error(y5 <- bain(fit5,hypotheses5,standardize = TRUE)))

# standardize is false met between group constraints
model6 <- 'postnumb ~ c(v2,v2)*age + peabody + prenumb + funumb + viewenc'
sesamesim$sex <- factor(sesamesim$sex, labels = c("boy", "girl"))
fit6 <- sem(model6, data = sesamesim, std.lv = TRUE, group = "sex")
hypotheses6 <- "postnumb~viewenc.girl > 0"
set.seed(100)
test_that("equal constraints return error", expect_error(y6 <- bain(fit6,hypotheses6,standardize = FALSE)))


# single group 1 equality constraint - constraint on restricted pars, standardize = true
model9 <- 'postnumb ~ v2*age + v2*peabody + sex'
fit9 <- sem(model9, data = sesamesim, std.lv = TRUE)
hypotheses9 <- "v2 > 0"
set.seed(100)
test_that("equal constraints return error", expect_error(y9 <- bain(fit9,hypotheses9,standardize = TRUE)))

# single group 1 equality constraint - constraint on unrestricted pars, standardize = true
model11 <- 'postnumb ~ v2*age + v2*peabody + sex'
fit11 <- sem(model11, data = sesamesim, std.lv = TRUE)
hypotheses11 <- "postnumb ~ sex > 0"
set.seed(100)
test_that("equal constraints return error", expect_error(y11 <- bain(fit11,hypotheses11,standardize = TRUE)))

# single group 1 equality constraint for three pars- constraint on restricted pars, std = true
model12 <- 'postnumb ~ v2*age + v2*peabody + v2*sex'
fit12 <- sem(model12, data = sesamesim, std.lv = TRUE)
hypotheses12 <- "v2 > 0"
set.seed(100)
test_that("equal constraints return error", expect_error(y12 <- bain(fit12,hypotheses12,standardize = TRUE)))


# single group 2 equality constraints- constraint on restricted pars, std = true
model13 <- 'postnumb ~ v2*age + v2*peabody + v3*sex + v3*prenumb + funumb'
fit13 <- sem(model13, data = sesamesim, std.lv = TRUE)
hypotheses13 <- "v2 > 0; v3>0"
set.seed(100)
test_that("equal constraints return error", expect_error(y13 <- bain(fit13,hypotheses13,standardize = TRUE)))



# single group 1 equality constraint for three pars- constraint on restricted pars, std = false
model14 <- 'postnumb ~ v2*age + prenumb + v2*peabody + v2*sex + funumb'
fit14 <- sem(model14, data = sesamesim, std.lv = TRUE)
hypotheses14 <- "v2 > 0 & postnumb~prenumb >0"
set.seed(100)
y14<-bain(fit14,hypotheses14,standardize = FALSE)
covariance14 <- lavInspect(fit14,"vcov")[c(1,2,5),c(1,2,5)]
test_that("Bain mutual", 
          {expect_equal(as.vector(y14$posterior), as.vector(covariance14) )}         )





# standardize is false met within group constraints
model7 <- 'postnumb ~ c(v2,v3)*age + c(v2,v3)*peabody + c(v4,v5)*prenumb + c(v4,v5)*funumb + viewenc'
sesamesim$sex <- factor(sesamesim$sex, labels = c("boy", "girl"))
fit7 <- sem(model7, data = sesamesim, std.lv = TRUE, group = "sex")
hypotheses7 <- "postnumb~viewenc.girl > 0"
set.seed(100)
y7 <- bain(fit7,hypotheses7,standardize = FALSE)
covariance7 <- lavInspect(fit7,"vcov")[c(1,3,5,7,8,10,12,14),c(1,3,5,7,8,10,12,14)]
test_that("Bain mutual", {expect_equal(as.vector(y7$posterior), as.vector(covariance7) )}         )

# standardize is false met within group constraints die drie 
# regressie coefficienten gelijk stellen
model8 <- 'postnumb ~ c(v2,v3)*age + c(v2,v3)*peabody + c(v2,v3)*prenumb +funumb + viewenc'
sesamesim$sex <- factor(sesamesim$sex, labels = c("boy", "girl"))
fit8 <- sem(model8, data = sesamesim, std.lv = TRUE, group = "sex")
hypotheses8 <- "postnumb~viewenc.girl > 0 & v2 > 0"
set.seed(100)
y8 <- bain(fit8,hypotheses8,standardize = FALSE)
covariance8 <- lavInspect(fit8,"vcov")[c(1,4,5,7,8,11,12,14),c(1,4,5,7,8,11,12,14)]
test_that("Bain mutual", 
          {expect_equal(as.vector(y8$posterior), as.vector(covariance8) )}         )


