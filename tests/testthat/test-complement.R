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
# the dependent variable is influence, it was simulated in accordance
# with the descriptives presented in Lucas (2003)
# group=1: randomly selected male leader
# group=2: randomly selected female leader
# group=3: task based selected male leader
# group=4: task based selected female leader
# group=5: institutionalized task based female leader

# =========================================================================
# TESTING THAT DIM AND NROW RENDER NULL FOR MATRIX WITH ONE ROW
Rrres <- bain:::parse_hypothesis(c("a","b","c"),"a > b > c")
x <- Rrres$hyp_mat[[1]][-1,]
test_that("PMPc", {expect_equal(c(nrow(x),dim(x)),c(NULL,NULL)    )})
# TESTING THAT DIM AND NROW RENDER 0 FOR MATRIX WITH zero ROWS
y <-  Rrres$hyp_mat[[1]][c(-1,-2),]
test_that("PMPc", {expect_equal(c(nrow(y),dim(y)),c(0,0,4)    )})

# ==============================================================================

# TEST 1a: ONE HYPOTHESIS  =, CALL WITH LM, FRACTION UNSPECIFIED
anov <- lm(influence~group-1,ldata)
set.seed(99)
results2 <- bain(anov, "group1 = group2 = group3")
test_that("PMPc", {expect_equal(results2$fit$PMPb[c(1,2)] , results2$fit$PMPc[c(1,3)])})

# ==============================================================================

# TEST 1b: TWO HYPOTHESES, BOTH CONTAINING =, CALL WITH LM, FRACTION UNSPECIFIED
anov <- lm(influence~group-1,ldata)
set.seed(99)
results2 <- bain(anov, "group1 = group2 = group3; group3 > group4 = group5")
test_that("PMPc", {expect_equal(results2$fit$PMPb[c(1,2,3)] , results2$fit$PMPc[c(1,2,4)])})

# ==============================================================================

# TEST 2a: ONE HYPOTHESIS, ><, CALL WITH LM, FRACTION SPECIFIED
anov <- lm(influence~group-1,ldata)
set.seed(110)
results2 <- bain(anov, "group3 > group4 > (group2, group1)", fraction = 5)
test_that("PMPc", {expect_equal(results2$fit$BF.u[c(1,3)]/sum(results2$fit$BF.u[c(1,3)]),
                                results2$fit$PMPc[c(1,3)])})
# ==============================================================================

# TEST 2e: ONE HYPOTHESIS, ><, CALL WITH LM, FRACTION SPECIFIED, TWICE THE SAME
# FIRST ORDER HYPOTHESIS

anov <- lm(influence~group-1,ldata)
set.seed(110)
results3 <- bain(anov, "group3 > group4 > (group2, group1);group3 > group4 > (group2, group1)", fraction = 5)
test_that("PMPc", {expect_equal(results2$fit$Fit[3],results3$fit$Fit[4],tolerance = .009)})
test_that("PMPc", {expect_equal(results2$fit$Com[3],results3$fit$Com[4],tolerance = .009)})

# ==============================================================================

# TEST 2b: ONE INCONSISTENT HYPOTHESIS, ><, CALL WITH LM, FRACTION SPECIFIED
anov <- lm(influence~group-1,ldata)
set.seed(110)
results2 <- bain(anov, "group1 > group2 > group3 & group1 < group3", fraction = 5)
test_that("PMPc", {expect_equal(results2$fit$PMPc,c(NaN,NA,NaN))})

# ==============================================================================

# TEST 2c: ONE INCONSISTENT HYPOTHESIS >< AND 1 =, CALL WITH LM, FRACTION SPECIFIED
anov <- lm(influence~group-1,ldata)
set.seed(110)
results2 <- bain(anov, "group1 > group2 > group3 & group1 < group3;
                       group1 = group2 = group3", fraction = 5)
test_that("PMPc", {expect_equal(results2$fit$PMPc,c(NaN,NaN,NA,NaN))})

# ==============================================================================

# TEST 2d: ONE INCONSISTENT HYPOTHESIS >< AND 1 =, CALL WITH LM, FRACTION SPECIFIED
anov <- lm(influence~group-1,ldata)
set.seed(99)
results2 <- bain(anov, "group1 > group2 = group3 & group1 < group2;
                       ", fraction = 5)
test_that("PMPc", {expect_equal(results2$fit$PMPc,c(NaN,NA,NaN))})

# ==============================================================================

# TEST 3: THREE HYPOTHESES, ONE ><, TWO ==, CALL WITH LM, FRACTION SPECIFIED
anov <- lm(influence~group-1,ldata)
set.seed(110)
results2 <- bain(anov, "group3 > group4 > (group2, group1);
                group3 = group4 = group5; group1 = group2", fraction = 5)
# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[5],results2$fit$Com[5]),
                                c(1 - results2$fit$Fit[1],
                                  1 - results2$fit$Com[1])   )})
# test BFcu
test_that("BFcu", {expect_equal(results2$fit$BF.u[5],results2$fit$Fit[5]/results2$fit$Com[5])})
# test PMPc
test_that("PMPc", {expect_equal(  results2$fit$BF.u[c(1,2,3,5)]/sum(results2$fit$BF.u[c(1,2,3,5)]),
                                  results2$fit$PMPc[c(1,2,3,5)]  )  })

# ==============================================================================

# TEST 4a: Two >< HYPOTHESES, CALL WITH LM
anov <- lm(influence~group-1,ldata)
set.seed(110)
results2 <- bain(anov, "group3 > group4 > (group2, group1);
                group5 > (group2, group1)")
set.seed(110)
# restest <- bain(anov, "group3 > group4 > (group2, group1) &
#                 group5 > (group2, group1)")

restest <- c(.396,.052)

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[4],results2$fit$Com[4]),
                                c(1 - (sum(results2$fit$Fit[1:2]) - restest[1]),
                                  1 - (sum(results2$fit$Com[1:2]) - restest[2])) , tolerance = .004)})
# test BFcu
test_that("BFcu", {expect_equal(results2$fit$BF.u[4],results2$fit$Fit[4]/results2$fit$Com[4])})
# test PMPc
test_that("PMPc", {expect_equal(results2$fit$BF.u[c(1,2,4)]/sum(results2$fit$BF.u[c(1,2,4)]),
                                results2$fit$PMPc[c(1,2,4)])})

# ==============================================================================

# TEST 4b: Two >< HYPOTHESES, CALL WITH LM
anov <- lm(influence~group-1,ldata)
set.seed(110)
results2 <- bain(anov, "group1 > group2 > group3 ; group3 > group2 > group1")

# set.seed(110)
# restest <- bain(anov, "group1 > group2 > group3 & group3 > group2 > group1")
restest <- c(0,0)

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[4],results2$fit$Com[4]),
                                c(1 - (sum(results2$fit$Fit[1:2]) - restest[1]),
                                  1 - (sum(results2$fit$Com[1:2]) - restest[2])) )})
# test BFcu
test_that("BFcu", {expect_equal(results2$fit$BF.u[4],results2$fit$Fit[4]/results2$fit$Com[4])})
# test PMPc
test_that("PMPc", {expect_equal(results2$fit$BF.u[c(1,2,4)]/sum(results2$fit$BF.u[c(1,2,4)]),
                                results2$fit$PMPc[c(1,2,4)])})

# ==============================================================================

# TEST 4c: Two >< HYPOTHESES, CALL WITH LM
anov <- lm(influence~group-1,ldata)
set.seed(110)
results2 <- bain(anov, "group1 > group2 > group3 > group4 ;
                       group1 > group3 & group2 > group4")
# set.seed(110)
# restest <- bain(anov, "group1 > group2 > group3 > group4 &
#                        group1 > group3 & group2 > group4")

restest <- c(0,.04)

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[4],results2$fit$Com[4]),
                                c(1 - (sum(results2$fit$Fit[1:2]) - restest[1]),
                                  1 - (sum(results2$fit$Com[1:2]) - restest[2])), tolerance = .003 )})
# test BFcu
test_that("BFcu", {expect_equal(results2$fit$BF.u[4],results2$fit$Fit[4]/results2$fit$Com[4])})
# test PMPc
test_that("PMPc", {expect_equal(results2$fit$BF.u[c(1,2,4)]/sum(results2$fit$BF.u[c(1,2,4)]),
                                results2$fit$PMPc[c(1,2,4)])})


# ==============================================================================

# TEST 5: Two >< HYPOTHESES AND TWO = HYPOTHESES, CALL WITH LM
anov <- lm(influence~group-1,ldata)
set.seed(110)
results2 <- bain(anov, "group3 > group4 > (group2, group1);
                       group3 = group4 = group2 = group1;
                       group5 > (group2, group1);
                       group5 = group2 = group1")
# set.seed(110)
# restest <- bain(anov, "group3 > group4 > (group2, group1) &
#                 group5 > (group2, group1)")
restest <- c(.396,.052)

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[6],results2$fit$Com[6]),
                                c(1 - (sum(results2$fit$Fit[c(1:3)]) - restest[1]),
                                  1 - (sum(results2$fit$Com[c(1:3)]) - restest[2])), tolerance = .008 )})
# test BFcu
test_that("BFcu", {expect_equal(results2$fit$BF.u[6],results2$fit$Fit[6]/results2$fit$Com[6])})
# test PMPc
test_that("PMPc", {expect_equal(results2$fit$BF.u[c(1,2,3,4,6)]/sum(results2$fit$BF.u[c(1,2,3,4,6)]),
                                results2$fit$PMPc[c(1,2,3,4,6)])})

# ==============================================================================

# TEST 6: Four >< HYPOTHESES, CALL WITH LM
anov <- lm(influence~group-1,ldata)
set.seed(110)
results2 <- bain(anov, "group3 > group4 > (group2, group1);
                       group5 > (group2, group1);
                       group5 > group3;
                       group5 > group2")

# set.seed(110)
# restest <- bain(anov, "group3 > group4 > (group2, group1) & group5 > (group2, group1);
#                        group3 > group4 > (group2, group1) & group5 > group3;
#                        group3 > group4 > (group2, group1) & group5 > group2;
#                        group5 > (group2, group1) & group5 > group3;
#                        group5 > (group2, group1) & group5 > group2;
#                        group5 > group3 & group5 > group2;
#                        group3 > group4 > (group2, group1)&
#                        group5 > group3&
#                        group5 > group2;
#                        group3 > group4 > (group2, group1)&
#                        group5 > (group2, group1)&
#                        group5 > group2;
#                        group3 > group4 > (group2, group1)&
#                        group5 > (group2, group1)&
#                        group5 > group3;
#                        group5 > (group2, group1) &
#                        group5 > group3 &
#                        group5 > group2;
#                        group3 > group4 > (group2, group1)&
#                        group5 > (group2, group1)&
#                        group5 > group3&
#                        group5 > group2")

resf <- c(.396,.205,.393,.535,.987,.526,.204,.395,.205,.526,.203)
resc <- c(.052,.017,.058,.254,.330,.333,.017,.051,.017,.250,.017)

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[6],results2$fit$Com[6]),
                                c(1 - (sum(results2$fit$Fit[1:4])
                                       - sum(resf[1:6])
                                       + sum(resf[7:10])
                                       - sum(resf[11])),
                                  1 - (sum(results2$fit$Com[1:4])
                                       - sum(resc[1:6])
                                       + sum(resc[7:10])
                                       - sum(resc[11]  )
                                  )), tolerance = .025           )})

# test BFcu
test_that("BFcu", {expect_equal(results2$fit$BF.u[6],results2$fit$Fit[6]/results2$fit$Com[6])})
# test PMPc
test_that("PMPc", {expect_equal(results2$fit$BF.u[c(1,2,3,4,6)]/sum(results2$fit$BF.u[c(1,2,3,4,6)]),
                                results2$fit$PMPc[c(1,2,3,4,6)])})

# ==============================================================================

# TEST 7a: Two >< Hypotheses, one about equal hypothesis, CALL WITH LM
anov <- lm(influence~group-1,ldata)
set.seed(76)
results2 <- bain(anov, "group3 > group4 > (group2, group1);
                       group5 > (group2, group1); -1 < group5 - group3 < 1")
# set.seed(110)
# restest <- bain(anov, "group3 > group4 > (group2, group1) &
#                        group5 > (group2, group1);
#                        group3 > group4 > (group2, group1) &
#                        -1 < group5 - group3 < 1;
#                        group5 > (group2, group1) &
#                        -1 < group5 - group3 < 1;
#                        group3 > group4 > (group2, group1) &
#                        group5 > (group2, group1) &
#                        -1 < group5 - group3 < 1")

resf <- c(.396,.392,.975,.387)
resc <- c(.052,.024,.110,.023)

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[5],results2$fit$Com[5]),
                                c(1 - (sum(results2$fit$Fit[1:3])
                                       - sum(resf[1:3])
                                       + sum(resf[4]) ),
                                  1 - (sum(results2$fit$Com[1:3])
                                       - sum(resc[1:3])
                                       + sum(resc[4])
                                  ))   , tolerance = .008       )})

# test BFcu
test_that("BFcu", {expect_equal(results2$fit$BF.u[5],results2$fit$Fit[5]/results2$fit$Com[5])})
# test PMPc
test_that("PMPc", {expect_equal(results2$fit$BF.u[c(1,2,3,5)]/sum(results2$fit$BF.u[c(1,2,3,5)]),
                                results2$fit$PMPc[c(1,2,3,5)])})

# ==============================================================================

# TEST 7b: Two >< Hypotheses, one about equal hypothesis, CALL WITH LM
#          TEST MET REDUNDANTIE HYPOTHESES
anov <- lm(influence~group-1,ldata)
set.seed(99)
results3 <- bain(anov, "group3 > group4 > (group2, group1);
                       group5 > (group2, group1); -1 < group5 - group3 < 1;
                       group3 > group4 > (group2, group1);
                       group5 > (group2, group1); -1 < group5 - group3 < 1")


# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[5],results2$fit$Com[5]),
                                c(results3$fit$Fit[8],results3$fit$Com[8]), tolerance = .038
)})

# ==============================================================================

# TEST 8: Two >< HYPOTHESES WITH CONFLICTING CONSTRAINTS, CALL WITH LM

anov <- lm(influence~group-1,ldata)
set.seed(99)
results2 <- bain(anov, "group3 > group4 > group5;
                       group5 > group4 > group3")
# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[4],results2$fit$Com[4]),
                                c(1 - (sum(results2$fit$Fit[1:2]) ),
                                  1 - (sum(results2$fit$Com[1:2]) ))     )})

# test BFcu
test_that("BFcu", {expect_equal(results2$fit$BF.u[4],results2$fit$Fit[4]/results2$fit$Com[4])})
# test PMPc
test_that("PMPc", {expect_equal(results2$fit$BF.u[c(1,2,4)]/sum(results2$fit$BF.u[c(1,2,4)]),
                                results2$fit$PMPc[c(1,2,4)])})

# ==============================================================================

# TEST 9: Two >< HYPOTHESES WITH CONFLICTING CONSTRAINTS AND TWO = HYPOTHESES, CALL WITH LM
anov <- lm(influence~group-1,ldata)
set.seed(99)
results2 <- bain(anov, "group3 > group4 > group5;
                       group5 > group4 > group3;
                       group3 = group4 = group5;
                       group3 = group5")

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[6],results2$fit$Com[6]),
                                c(1 - (sum(results2$fit$Fit[1:2]) ),
                                  1 - (sum(results2$fit$Com[1:2]) ))     )})

# test BFcu
test_that("BFcu", {expect_equal(results2$fit$BF.u[6],results2$fit$Fit[6]/results2$fit$Com[6])})
# test PMPc
test_that("PMPc", {expect_equal(results2$fit$BF.u[c(1,2,3,4,6)]/sum(results2$fit$BF.u[c(1,2,3,4,6)]),
                                results2$fit$PMPc[c(1,2,3,4,6)])})

# ==============================================================================

# TEST 10: Two >< HYPOTHESES WITH CONFLICTING CONSTRAINTS AND TWO = HYPOTHESES,
# TESTING THE SCALE FACTOR, CALL WITH LM
anov <- lm(influence~group-1,ldata)
set.seed(99)
results3 <- bain(anov, "group3 > group4 > group5;
                       2 * group5 > 2 * group4 > 2 * group3;
                       group3 = group4 = group5;
                       group3 = group5")

# TEST VERSUS TEST9
test_that("PMPc", {expect_equal(results2$fit, results3$fit)})

# ==============================================================================

# TEST 11: Two >< HYPOTHESES WITH CONSTRAINTS THAT CONCLICT AFTER SUMMING
# AND TWO = HYPOTHESES, TESTING THE SCALE FACTOR, CALL WITH LM
anov <- lm(influence~group-1,ldata)
set.seed(99)
results2 <- bain(anov, "5*group3 > 5*group4 > 5*group5;
                       2* group5 > 2*group3;
                       group3 = group4 = group5;
                       group4 = group5")

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[6],results2$fit$Com[6]),
                                c(1 - (sum(results2$fit$Fit[1:2]) ),
                                  1 - (sum(results2$fit$Com[1:2]) ))     )})

# test BFcu
test_that("BFcu", {expect_equal(results2$fit$BF.u[6],results2$fit$Fit[6]/
                                  results2$fit$Com[6])})
# test PMPc
test_that("PMPc", {expect_equal(results2$fit$BF.u[c(1,2,3,4,6)]/
                                  sum(results2$fit$BF.u[c(1,2,3,4,6)]),
                                results2$fit$PMPc[c(1,2,3,4,6)])})

# ==============================================================================

# TEST 12: TWO TIMES TWO CONFLICTING >< AND TWO = WITH ONE SC, AND LM

anov <- lm(influence~group-1,ldata)
set.seed(99)
results2 <- bain(anov, "5*group3 > 5*group4 > 5*group5;
                       2* group5 > 2*group3;
                       group1 > group2;
                       group2 > group1;
                       group3 = group4 = group5;
                       group3 = group5")

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[8],results2$fit$Com[8]),
                                c(0,0)   , tolerance = .005  )})

# ==============================================================================

# TEST 13: TWO TIMES TWO CONFLICTING >< AND TWO = WITH ONE SC, AND LM
# AND FRACTION

anov <- lm(influence~group-1,ldata)
set.seed(99)
results2 <- bain(anov, "5*group3 > 5*group4 > 5*group5;
                       2* group5 > 2*group3;
                       group1 > group2;
                       group2 > group1;
                       group3 = group4 = group5;
                       group3 = group5", fraction = 10)

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[8],results2$fit$Com[8]),
                                c(0,0)   , tolerance = .005  )})


# ==============================================================================

# TEST 14: ONE >< ONE ==, LM AND STANDARDIZE = TRUE

anov <- lm(postnumb~funumb+prenumb,sesamesim)
set.seed(99)
results2 <- bain(anov, "funumb = 0 & prenumb = 0; funumb > 0 & prenumb > 0", standardize = TRUE)

# JOINTFIT, JOINTCOM
test_that("PMPc", {expect_equal(  c(results2$fit$Fit[4],results2$fit$Com[4]),
                                  c(1-results2$fit$Fit[2],1-results2$fit$Com[2])     )})


# ==============================================================================

# TEST 15: ONE >< ONE ==, LM AND STANDARDIZE = IMPLICITLY FALSE

anov <- lm(postnumb~funumb+prenumb,sesamesim)
set.seed(99)
results2 <- bain(anov, "funumb = 0 & prenumb = 0; funumb > 0 & prenumb > 0")


# JOINTFIT, JOINTCOM
test_that("PMPc", {expect_equal(  c(results2$fit$Fit[4],results2$fit$Com[4]),
                                  c(1-results2$fit$Fit[2],1-results2$fit$Com[2])     )})


# ==============================================================================
# TEST 16: Two >< HYPOTHESES, CALL WITH LM, ALL POSSIBLE ORDERINGS OF TWO MEANS

anov <- lm(influence~group-1,ldata)
set.seed(99)
results2 <- bain(anov, "group1 > group2;
                       group2 > group1")

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[4],results2$fit$Com[4]),
                                c(0,0)   , tolerance = .005  )})

# ==============================================================================

# TEST 17: Six >< HYPOTHESES, CALL WITH LM, ALL POSSIBLE ORDERINGS OF THREE MEANS

anov <- lm(influence~group-1,ldata)
set.seed(99)
results2 <- bain(anov, "group1 > group2 > group3;
                       group1 > group3 > group2;
                       group2 > group1 > group3;
                       group2 > group3 > group1;
                       group3 > group1 > group2;
                       group3 > group2 > group1")

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[8],results2$fit$Com[8]),
                                c(0,0)   , tolerance = .006  )})
# ==============================================================================

# TEST 18: Three >< HYPOTHESES, CALL WITH LM, parameter space is overcovered

anov <- lm(influence~group-1,ldata)
set.seed(99)
results2 <- bain(anov, "group1 > group2;
                       group1 < 0;
                       group1 > 0")

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[5],results2$fit$Com[5]),
                                c(0,0)   , tolerance = .004  )})

# ==============================================================================

# TEST 19: lavaan

model1 <- '
    A =~ Ab + Al + Af + An + Ar + Ac
    B =~ Bb + Bl + Bf + Bn + Br + Bc
'

fit1 <- lavaan::sem(model1, data = sesamesim, std.lv = TRUE)

hypotheses1 <-
  " A=~Ab > .6 & A=~Al > .6 & A=~Af > .6 ; A=~An = .6 & A=~Ar = .6 & A=~Ac=.6 ;
B=~Bb > .6 & B=~Bl > .6 & B=~Bf > .6 ; B=~Bn = .6 & B=~Br = .6 & B=~Bc =.6"
set.seed(100)
results2 <- bain(fit1,hypotheses1,fraction=4,standardize=TRUE)

# restest <- bain(fit1,"A=~Ab > .6 & A=~Al > .6 & A=~Af > .6 & B=~Bb > .6 & B=~Bl > .6 & B=~Bf > .6",fraction=4,standardize=TRUE)

resf <- c(.880)
resc <- c(.050)

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[6],results2$fit$Com[6]),
                                c(1-(results2$fit$Fit[1]+results2$fit$Fit[3]-resf),
                                  1-(results2$fit$Com[1]+results2$fit$Com[3]-resc) )  , tolerance = .004  )})


# ==============================================================================

# TEST 20: T-TEST

x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]
ttest <- t_test(x,y, var.equal = FALSE)
set.seed(100)
results2 <- bain(ttest, "x = y; x > y")

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[4],results2$fit$Com[4]),
                                c(1-(results2$fit$Fit[2]),
                                  1-(results2$fit$Com[2]) )  , tolerance = .002  )})

# ==============================================================================

# TEST 21: THREE HYPOTHESES, TWO ><, ONE ==, CALL WITH BAIN DEFAULT

sesamesim$sex <- factor(sesamesim$sex)
bw <- lm(cbind(prenumb, postnumb, funumb)~sex-1, data=sesamesim)
est1 <-coef(bw)[1,1:3] # the three means for sex = 1
est2 <-coef(bw)[2,1:3] # the three means for sex = 2
estimate <- c(est1,est2)
names(estimate) <- c("pre1", "post1", "fu1","pre2", "post2", "fu2")
ngroup<-table(sesamesim$sex)
cov1 <- c(vcov(bw)[1,1],vcov(bw)[1,3],vcov(bw)[1,5],vcov(bw)[3,1],
          vcov(bw)[3,3],vcov(bw)[3,5],vcov(bw)[5,1],vcov(bw)[5,3],vcov(bw)[5,5])
cov1 <- matrix(cov1,3,3)
cov2 <- c(vcov(bw)[2,2],vcov(bw)[2,4],vcov(bw)[2,6],vcov(bw)[4,2],
          vcov(bw)[4,4],vcov(bw)[4,6],vcov(bw)[6,2],vcov(bw)[6,4],vcov(bw)[6,6])
cov2 <- matrix(cov2,3,3)
covariance<-list(cov1,cov2)
set.seed(100)
results2 <-bain(estimate, "pre1 - pre2 = post1 - post2 = fu1 -fu2;
pre1 - pre2 > post1 - post2 > fu1 -fu2;
fu1 -fu2 > post1 - post2 > pre1 - pre2"  , n=ngroup, Sigma=covariance,
                group_parameters=3, joint_parameters = 0)

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[5],results2$fit$Com[5]),
                                c(1-(results2$fit$Fit[2]+results2$fit$Fit[3]),
                                  1-(results2$fit$Com[2]+results2$fit$Com[3]) )  , tolerance = .002  )})

# ==============================================================================

# TEST 22: FOUR >< SUMMING WHOLE PAR SPACE

anov <- lm(postnumb~funumb+prenumb,sesamesim)
set.seed(99)
results2 <- bain(anov, "funumb > 0 & prenumb > 0; funumb < 0 & prenumb > 0;
                funumb > 0 & prenumb < 0; funumb < 0 & prenumb < 0")

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[6],results2$fit$Com[6]),
                                c(0,0)  , tolerance = .002  )})

# ==============================================================================

# TEST 23: ONE HYPOTHESIS  CALL WITH LM, FRACTION UNSPECIFIED, TESTING ABOUTS
# MULTIPLE, DUPLICATES, CONSISTENTS, INCONSISTENTS ALREADY IN THE FIRST
# ORDER HYPOTHESES
anov <- lm(influence~group-1,ldata)
set.seed(99)
results2 <- bain(anov, "2 < group1 < 2.5; 2 < group1 < 2.5; 2 < group1 < 2.5;
                2 > group1 > 2.5; 1<  group2 < 2")

# restest <- bain(anov, "2<group1<2.5&2<group1<2.5;
#                  2<group1<2.5&2<group1<2.5;
#                  2<group1<2.5&1<group2<2;
#                  2<group1<2.5&2<group1<2.5;
#                  2<group1<2.5&1<group2<2;
#                  2<group1<2.5&1<group2<2;
#                  2<group1<2.5&2<group1<2.5&2<group1<2.5;
#                  2<group1<2.5&2<group1<2.5&1<group2<2;
#                  2<group1<2.5&2<group1<2.5&1<group2<2;
#                  2<group1<2.5&2<group1<2.5&1<group2<2;
#                  2<group1<2.5&2<group1<2.5&2<group1<2.5&1<group2<2")

resf <- c(.601,.599,.519,.604,.526,.521,.593,.510,.513,.518,.523)
resc <- c(.096,.095,.019,.098,.018,.019,.100,.018,.020,.021,.019)

test_that("PMPc", {expect_equal(c(results2$fit$Fit[7],results2$fit$Com[7]),
                                c(1 - (sum(results2$fit$Fit[1:5]) - sum(resf[1:6])
                                       + sum(resf[7:10]) - resf[11]),1 - (sum(results2$fit$Com[1:5]) - sum(resc[1:6])
                                                                          + sum(resc[7:10]) - resc[11]))   ,tolerance = .05  )})

# ==============================================================================

# TEST 24a: TESTING ABOUTS AND ORDER

anov <- lm(influence~group-1,ldata)
set.seed(997)
results2 <- bain(anov, "2 < group1 < 3 & 3 <  group3 < 4 &
                       2.5 < group5 < 3.5 ;
                       group1 < group5 - .5 < group3 -.5")
set.seed(99)
# restest <- bain(anov, "2 < group1 < 3 & 3 <  group3 < 4 &
#                        2.5 < group5 < 3.5 &
#                        group1 < group5 - .5 < group3 -.5")

resf <- .266
resc <- .004

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[4],results2$fit$Com[4]),
                                c(1-(results2$fit$Fit[1]+results2$fit$Fit[2]-resf[1]) ,
                                  1-(results2$fit$Com[1]+results2$fit$Com[2]-resc[1]) ), tolerance = .006
)})

# ==============================================================================

# TEST 24b: TESTING ABOUTS

anov <- lm(influence~group-1,ldata)
set.seed(99)
results2 <- bain(anov, "group1 > 0 & group1 < 2; group1 < group2 + .5; group1 > group2 + .5")
set.seed(99)
# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[5],results2$fit$Com[5]),
                                c(0,0 )  , tolerance = .009
)})

# ==============================================================================

# TEST 24c: TESTING ABOUTS

anov <- lm(influence~group-1,ldata)
set.seed(99)
results2 <- bain(anov, "
-1 < group1 - group2 < 1 &  -1 < group1 - group3 < 1 &  -1 < group2 - group3 < 1
                ")
# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[3],results2$fit$Com[3]),
                                c(1-results2$fit$Fit[1],1-results2$fit$Com[1])
)})

# ==============================================================================

# TEST 24d: TESTING ABOUTS

anov <- lm(influence~group-1,ldata)
set.seed(99)
results2 <- bain(anov, "
-1 < group1 - group2 < 1 ;  -1 < group1 - group3 < 1 ;  -1 < group2 - group3 < 1
                ")

# set.seed(99)
# restest <- bain(anov, "
# -1 < group1 - group2 < 1 &  -1 < group1 - group3 < 1 ;
# -1 < group1 - group2 < 1 &  -1 < group2 - group3 < 1 ;
# -1 < group1 - group3 < 1 &  -1 < group2 - group3 < 1 ;
# -1 < group1 - group2 < 1 &  -1 < group1 - group3 < 1 &  -1 < group2 - group3 < 1
#                 ")

resf <- c(.233,.015,.015,.014)
resc <- c(.090,.085,.086,.069)

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[5],results2$fit$Com[5]),
                                c(1-(sum(results2$fit$Fit[1:3])- sum(resf[1:3]) + resf[4]),
                                  1-(sum(results2$fit$Com[1:3])- sum(resc[1:3]) + resc[4])
                                ) , tolerance = .01
)})

# ==============================================================================

# TEST 24e: TESTING ABOUTS

anov <- lm(influence~group-1,ldata)
set.seed(76)
results2 <- bain(anov, "
-1 < group1 - group2 < 1 ;  -1 < group1 - group3 < 1 ;  -1 > group2 - group3 > 1
                ")
resf <- .233
resc <- .090

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[5],results2$fit$Com[5]),
                                c(1-(sum(results2$fit$Fit[1:3])-  resf[1]),
                                  1-(sum(results2$fit$Com[1:3])-  resc[1])
                                ) , tolerance = .006
)})

# ==============================================================================

# # TEST 24f: TESTING ABOUTS
#
# anov <- lm(influence~group-1,ldata)
# set.seed(99)
# results2 <- bain(anov, "
# -1 < group1 - group2 < 1 &  -1 < group1 - group3 < 1 &  -1 < group2 - group3 < 1;
# -1 < group1 - group2 < 1 &  -1 < group2 - group3 < 1
#                 ")
#
# # test fit and complexity of the complement
# test_that("fc", {expect_equal(  c(results2$fit$Fit[4],results2$fit$Com[4]),
#                                 c(1-results2$fit$Fit[2],
#                                   1-results2$fit$Com[2]
#                                 ) , tolerance = .003
# )})

# ==============================================================================

# TEST 24g: TESTING ABOUTS WITH DUPLICATES OF THE CONSTRAINTS

anov <- lm(influence~group-1,ldata)
set.seed(99)
results2 <- bain(anov, "
-1 < group1 - group2 < 1 &  -1 < group1 - group3 < 1;  &
-1 < group1 - group2 < 1 &  -1 < group1 - group3 < 1
                ")
# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[4],results2$fit$Com[4]),
                                c(1-results2$fit$Fit[2],
                                  1-results2$fit$Com[2]
                                ) ,tolerance = .010
)})

# ==============================================================================

# TEST 24h: TESTING ABOUTS AND ORDER

anov <- lm(influence~group-1,ldata)
set.seed(99)
results2 <- bain(anov, "2 < group1 < 3 & 3 <  group3 < 4 &
                       2.5 < group5 < 3.5 ;
                       group1 < group5 - .5 < group3 -.5;
                       group1 > group5 - .5 > group3 -.5")
# set.seed(99)
# restest <- bain(anov, "2 < group1 < 3 & 3 <  group3 < 4 &
#                        2.5 < group5 < 3.5 &
#                        group1 < group5 - .5 < group3 -.5;
#                 2 < group1 < 3 & 3 <  group3 < 4 &
#                        2.5 < group5 < 3.5 &
#                        group1 > group5 - .5 > group3 -.5")


resf <- c(.266,.009)
resc <- c(.004,.000)

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[5],results2$fit$Com[5]),
                                c(1-sum(results2$fit$Fit[1:3]) + sum(resf[1:2]),
                                  1-sum(results2$fit$Com[1:3]) + sum(resc[1:2])
                                ), tolerance = .007
)})




# ==============================================================================

# TEST 25: TESTING ABOUTS VS PARS

anov <- lm(influence~group-1,ldata)
set.seed(99)
results2 <- bain(anov, "group1 < group2 < 2 * group1; -group1 < group2 < group1")
# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[4],results2$fit$Com[4]),
                                c(
                                  1-results2$fit$Fit[1]-results2$fit$Fit[2],
                                  1-results2$fit$Com[1]-results2$fit$Com[2]
                                )  , tolerance = .005
)})

# ==============================================================================

# TEST 26a: TESTING WITH REALISTIC SITUATIONS

anov <- lm(influence~group-1,ldata)
set.seed(99)
results2 <- bain(anov, "
group1 - group2 = group2 - group3 = group3 - group4;
group1 - group2 > (group2 - group3 , group3 - group4);
group1 - group2 > group2 - group3 > group3 - group4;
                ")
# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[5],results2$fit$Com[5]),
                                c(
                                  1-results2$fit$Fit[2],
                                  1-results2$fit$Com[2]
                                )  , tolerance = .007
)})

# ==============================================================================

# TEST 26b: TESTING WITH REALISTIC SITUATIONS

anov <- lm(influence~group-1,ldata)
set.seed(99)
results2 <- bain(anov, "
group1 - group2 = group2 - group3 = group3 - group4;
group1 - group2 > (group2 - group3 , group3 - group4);
group1 - group2 > group2 - group3 > group3 - group4;
group1 - group2 < (group2 - group3 , group3 - group4);
group1 - group2 > group2 - group3 < group3 - group4;
                ")

# set.seed(99)
# restest <- bain(anov, "
# group1-group2>(group2-group3,group3-group4)&group1-group2>group2-group3>group3-group4;
# group1-group2>(group2-group3,group3-group4)&group1-group2>group2-group3<group3-group4")

resf <- c(.000,.523)
resc <- c(.137,.181)

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[7],results2$fit$Com[7]),
                                c(1-sum(results2$fit$Fit[2:5])+sum(resf[1:2]),
                                  1-sum(results2$fit$Com[2:5])+sum(resc[1:2])
                                ) ,tolerance = .009
)})

# ==============================================================================

# TEST 26c: TESTING WITH REALISTIC SITUATIONS

anov <- lm(influence~group-1,ldata)
set.seed(999)
results2 <- bain(anov, "
group1 - group2 + 1 = 2 * group2 - 2 * group3 + 4= 3 * group3 - 3 *group4 + 2;
group1 - group2 + 1 > (2 * group2 - 2 * group3 + 4, 3 * group3 - 3 *group4 + 2);
group1 - group2 + 1 > 2 * group2 - 2 * group3 + 4> 3 * group3 - 3 *group4 + 2;
group1 - group2 + 1 < (2 * group2 - 2 * group3 + 4, 3 * group3 - 3 *group4 + 2);
group1 - group2 + 1 > 2 * group2 - 2 * group3 + 4< 3 * group3 - 3 *group4 + 2
                ")

# set.seed(99)
# restest <- bain(anov, "
# group1 - group2 + 1 > (2 * group2 - 2 * group3 + 4, 3 * group3 - 3 *group4 + 2)&
# group1 - group2 + 1 > 2 * group2 - 2 * group3 + 4> 3 * group3 - 3 *group4 + 2;
# group1 - group2 + 1 > (2 * group2 - 2 * group3 + 4, 3 * group3 - 3 *group4 + 2)&
# group1 - group2 + 1 > 2 * group2 - 2 * group3 + 4< 3 * group3 - 3 *group4 + 2
# ")

resf <- c(.001,.009)
resc <- c(.129,.100)

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[7],results2$fit$Com[7]),
                                c(1-sum(results2$fit$Fit[2:5])+sum(resf[1:2]),
                                  1-sum(results2$fit$Com[2:5])+sum(resc[1:2])
                                ) ,tolerance = .004
)})

# ==============================================================================

# TEST 26d: TESTING WITH REALISTIC SITUATIONS

anov <- lm(influence~group-1,ldata)
set.seed(999)
results2 <- bain(anov, "
group1 - group2 + 1 = 2 * group2 - 2 * group3 + 4= 3 * group3 - 3 *group4 + 2;
group1 - group2 + 1 > (2 * group2 - 2 * group3 + 4, 3 * group3 - 3 *group4 + 2);
group1 - group2 + 1 > 2 * group2 - 2 * group3 + 4> 3 * group3 - 3 *group4 + 2;
group1 - group2 + 1 < (2 * group2 - 2 * group3 + 4, 3 * group3 - 3 *group4 + 2);
group1 - group2 + 1 > 2 * group2 - 2 * group3 + 4< 3 * group3 - 3 *group4 + 2
                ")
# set.seed(99)
# restest <- bain(anov, "
# group1 - group2 + 1 > (2 * group2 - 2 * group3 + 4, 3 * group3 - 3 *group4 + 2)&
# group1 - group2 + 1 > 2 * group2 - 2 * group3 + 4> 3 * group3 - 3 *group4 + 2;
# group1 - group2 + 1 > (2 * group2 - 2 * group3 + 4, 3 * group3 - 3 *group4 + 2)&
# group1 - group2 + 1 > 2 * group2 - 2 * group3 + 4< 3 * group3 - 3 *group4 + 2
# ")

resf <- c(.001,.009)
resc <- c(.129,.100)

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[7],results2$fit$Com[7]),
                                c(1-sum(results2$fit$Fit[2:5])+sum(resf[1:2]),
                                  1-sum(results2$fit$Com[2:5])+sum(resc[1:2])
                                ),tolerance = .004
)})


# ==============================================================================

# TEST 26e: TESTING WITH REALISTIC SITUATIONS
# HET VOORBEELD VAN SASCHA

anov <- lm(influence~group-1,ldata)
set.seed(99)
results2 <- bain(anov, "group1 = group2 & group3 = group4;
                       group1 > group2 & group3 > group4;
                       group1 > group2 & group3 < group4")

# CHECK THAT BOTH >< HYPOTHESES ARE MUTUALLY EXCLUSIVE

# COMPUTE FIT OF COMPLEMENT
fitcompl <- 1 - sum(results2$fit$Fit[2:3])
# COMPUTE ?OM OF COMPLEMENT
comcompl <- 1 - sum(results2$fit$Com[2:3])

# test fit and complexity of the complement
test_that("fc", {expect_equal(  c(results2$fit$Fit[5],results2$fit$Com[5]),
                                c(fitcompl,comcompl)

)})

