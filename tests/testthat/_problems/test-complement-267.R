# Extracted from test-complement.R:267

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "bain", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
data(sesamesim)
ldata <- as.data.frame(matrix(0, 150, 2))
names(ldata)<-c("group","influence")
ldata$group <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                 4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
                 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5)
ldata$influence <- c(3.58,-0.15,0.67,2.22,2.56,1.70,-0.45,1.08,4.83,2.44,
                     6.74,1.40,-0.03,0.71,4.30,5.47,2.44,0.81,3.65,1.41,
                     -0.38,3.11,0.21,3.51,2.63,4.74,2.12,4.36,3.26,0.94,
                     1.67,1.85,0.50,0.63,0.04,2.80,0.02,1.32,-0.20,0.14,
                     2.65,0.07,3.00,0.53,-0.38,0.48,3.87,1.40,2.02,2.37,
                     1.99,1.32,0.86,2.30,-0.28,0.78,1.48,3.29,2.14,1.18,
                     1.39,4.53,1.18,3.67,2.88,3.50,1.74,5.43,4.69,1.62,
                     3.67,3.70,1.79,2.98,6.31,4.90,2.23,3.71,3.41,6.84,
                     2.88,-0.31,-0.08,4.14,3.45,5.20,2.20,-0.03,3.71,4.67,
                     1.57,2.97,1.45,1.78,0.97,4.30,5.12,1.67,1.65,1.66,
                     1.32,4.10,1.23,1.58,1.94,4.51,0.89,0.86,4.81,0.68,
                     0.47,0.56,2.62,2.41,1.49,5.01,3.94,2.69,2.10,0.57,
                     1.38,4.58,3.48,0.39,3.89,4.18,3.72,4.79,3.63,1.55,
                     -0.46,2.04,2.26,4.27,5.67,4.69,2.17,2.27,4.10,3.74,
                     4.54,1.71,4.74,4.21,4.10,4.80,2.90,2.35,4.09,1.09)
ldata$group <- as.factor(ldata$group)
Rrres <- bain:::parse_hypothesis(c("a","b","c"),"a > b > c")
x <- Rrres$hyp_mat[[1]][-1,]
y <-  Rrres$hyp_mat[[1]][c(-1,-2),]
anov <- lm(influence~group-1,ldata)
set.seed(99)
results2 <- bain(anov, "group1 = group2 = group3")
anov <- lm(influence~group-1,ldata)
set.seed(99)
results2 <- bain(anov, "group1 = group2 = group3; group3 > group4 = group5")
anov <- lm(influence~group-1,ldata)
set.seed(110)
results2 <- bain(anov, "group3 > group4 > (group2, group1)", fraction = 5)
anov <- lm(influence~group-1,ldata)
set.seed(110)
results3 <- bain(anov, "group3 > group4 > (group2, group1);group3 > group4 > (group2, group1)", fraction = 5)
anov <- lm(influence~group-1,ldata)
set.seed(110)
results2 <- bain(anov, "group1 > group2 > group3 & group1 < group3", fraction = 5)
anov <- lm(influence~group-1,ldata)
set.seed(110)
results2 <- bain(anov, "group1 > group2 > group3 & group1 < group3;
                       group1 = group2 = group3", fraction = 5)
anov <- lm(influence~group-1,ldata)
set.seed(99)
results2 <- bain(anov, "group1 > group2 = group3 & group1 < group2;
                       ", fraction = 5)
anov <- lm(influence~group-1,ldata)
set.seed(110)
results2 <- bain(anov, "group3 > group4 > (group2, group1);
                group3 = group4 = group5; group1 = group2", fraction = 5)
anov <- lm(influence~group-1,ldata)
set.seed(110)
results2 <- bain(anov, "group3 > group4 > (group2, group1);
                group5 > (group2, group1)")
set.seed(110)
restest <- c(.396,.052)
anov <- lm(influence~group-1,ldata)
set.seed(110)
results2 <- bain(anov, "group1 > group2 > group3 ; group3 > group2 > group1")
restest <- c(0,0)
anov <- lm(influence~group-1,ldata)
set.seed(110)
results2 <- bain(anov, "group1 > group2 > group3 > group4 ;
                       group1 > group3 & group2 > group4")
restest <- c(0,.04)
anov <- lm(influence~group-1,ldata)
set.seed(110)
results2 <- bain(anov, "group3 > group4 > (group2, group1);
                       group3 = group4 = group2 = group1;
                       group5 > (group2, group1);
                       group5 = group2 = group1")
restest <- c(.396,.052)
anov <- lm(influence~group-1,ldata)
set.seed(110)
results2 <- bain(anov, "group3 > group4 > (group2, group1);
                       group5 > (group2, group1);
                       group5 > group3;
                       group5 > group2")
resf <- c(.396,.205,.393,.535,.987,.526,.204,.395,.205,.526,.203)
resc <- c(.052,.017,.058,.254,.330,.333,.017,.051,.017,.250,.017)

# test -------------------------------------------------------------------------
expect_equal(results2$fit$BF.u[c(1,2,3,4,6)]/sum(results2$fit$BF.u[c(1,2,3,4,6)]),
                                results2$fit$PMPc[c(1,2,3,4,6)])
