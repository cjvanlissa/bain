# TESTING THE BAIN PARSER
# ALTHOUGH MORE IS POSSIBLE, THE USER IS REQUIRED TO USE STICK TO THE FOLLOWING SYNTAX:
# 1) COMBINATIONS OF number * par + number FOR MULTIPLE PARAMETERS
# 2) USE & TO COMBINE DIFFERENT PARTS OF AN HYPOTHESIS
# 3) USE (,) TO CREATE SET OF COMBINATIONS

rm(list=ls())

# TESTING THE NAMING OF THE PARAMETERS
# 1) USE OF LETTERS, . AND _
# 2) USE OF CAPITALS AND NUMBERS

varnames <- c("a._", "b_.", "c.c_")
hyp1 <- "a._ > b_. > c.c_"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(do.call(rbind, x$hyp_mat))), c(1,-1,0,0,0,1,-1,0))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0,2))})

varnames <- c("a2b", "b2G", "C")
hyp1 <- "a2b > b2G > C"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(do.call(rbind, x$hyp_mat))), c(1,-1,0,0,0,1,-1,0))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0,2))})

# TESTING THE USE OF LINEAR COMBINATIONS
# 1) SCALARS CONSISTING OF ONE OR TWO NUMBERS
# 2) SCALARS UP TO THREE NUMBERS, STARTING WITH . AND 0, NEGATIVE NUMBERS
# 3) A SINGLE "LONG" HYPOTHESIS
# 4) TESTS PROVIDED BY CASPAR
# 5) SERIES OF =, <, > IN ONE HYPOTHESIS
# 6) A SIMPLE EXAMPLE

varnames <- c("a", "b", "c")
hyp1 <- "a > 5 > c"
x<-bain:::parse_hypothesis(varnames, hyp1)

test_that("parser", {expect_equal(as.vector(t(do.call(rbind, x$hyp_mat))), c(1,0,0,5,0,0,-1,-5))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0,2))})




varnames <- c("a", "b", "c")
hyp1 <- "20<-21*a<4.5; a>b> c; 4a > 7; 4 + 7 > a - 4* b  + 8"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(do.call(rbind, x$hyp_mat))), c(-21,  0,  0 ,20.0,
                                                             21,  0,  0 ,-4.5,
                                                             1, -1,  0,  0.0,
                                                             0 , 1, -1,  0.0,
                                                             4,  0,  0,  7.0,
                                                             -1,  4 , 0, -3.0))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0, 2, 0, 2, 0, 1, 0, 1))})

varnames <- c("a22","b19")
hyp1 <- "12*a22 > 2; 243*b19 > .666; 5* a22 - 54*b19 < 45; -12*a22 > -2; 243*b19 < -0.666; 5* a22 - 54*b19 = 45"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(do.call(rbind, x$hyp_mat))), c(12,    0,   2.000,
                                                             0,  243 ,  0.666,
                                                             -5 ,  54 ,-45.000,
                                                             -12 ,   0 , -2.000,
                                                             0 ,-243 ,  0.666,
                                                             5 , -54,  45.000))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0))})

varnames <- c("a","b","c","d","e","f")
hyp1 <- "2* a -3*b -4*f + c > 2"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(do.call(rbind, x$hyp_mat))), c(2, -3, 1, 0, 0, -4, 2))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0,1))})

varnames <- c("a","b","coef1","coef2","coef_1")
hyp1<-"2=a;a=2; 2*a+2=4;2* a+4* b=5+7; 2*a=3+6*b; 2=coef1; coef1=2;2*coef1+2=4; 2*coef1+4*coef2=5+7;2=coef_1;coef_1=2"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(do.call(rbind, x$hyp_mat))), c(-1,  0,     0,     0 ,     0 ,-2,
                                                             1,  0,     0,     0 ,     0,  2,
                                                             2,  0 ,    0,     0,      0 , 2,
                                                             2 , 4,     0 ,    0 ,     0, 12,
                                                             2 ,-6 ,    0 ,    0 ,     0,  3,
                                                             0,  0 ,   -1 ,    0 ,     0, -2,
                                                             0,  0 ,    1 ,    0,      0,  2,
                                                             0,  0,     2,     0 ,     0 , 2,
                                                             0,  0,     2 ,    4 ,     0, 12,
                                                             0,  0,     0 ,    0 ,    -1, -2,
                                                             0,  0 ,    0 ,    0,      1,  2
))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0))})

varnames <- c("a","b","c", "d")
hyp1 <- "a > b < c > d; a=b > c < d; a > b-c = d;"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(do.call(rbind, x$hyp_mat))), c(1, -1,  0,  0, 0,
                                                             0, -1,  1,  0, 0,
                                                             0,  0,  1, -1, 0,
                                                             1, -1,  0 , 0, 0,
                                                             0,  1, -1,  0, 0,
                                                             0,  0, -1,  1, 0,
                                                             0,  1, -1, -1, 0,
                                                             1, -1,  1,  0, 0))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0, 3, 1, 2, 1, 1))})

varnames <- c("a","b","c","d","e","f")
hyp1 <- "2a -3b < 4f-c"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(do.call(rbind, x$hyp_mat))), c(-2, 3, -1, 0, 0, 4, 0))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0,1))})

# hypotheses specified using the &
# 1) STRAIGHTFORWARD AND WITH ONE USE OF (,)
# 2) SIMPLE
# 3) WRONG INPUT CORRECT RESULT
# 4) A SERIES OF &

varnames <- c("a1","a2","a3")
hyp1 <- "a1 = a2 & a2 = a3; (a1,a2) > a3 & a1 =a2"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(do.call(rbind, x$hyp_mat))), c(1, -1,  0, 0,
                                                             0,  1, -1, 0,
                                                             1, -1,  0, 0,
                                                             1,  0 ,-1, 0,
                                                             0,  1, -1, 0))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(2, 0, 1, 2))})

varnames <- c("a","b","c","d","e","f")
hyp1 <- "a=b&c=d"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(do.call(rbind, x$hyp_mat))), c(1,-1,0,0,0,0,0,0,0,1,-1,0,0,0))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(2,0))})

varnames <- c("a","b","c","d","e","f")
hyp1 <- "a&b=c&d"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(do.call(rbind, x$hyp_mat))), c(0, 1, -1, 0, 0, 0, 0))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(1,0))})

varnames <- c("a","b","c","d","e","f")
hyp1 <- "a = b & b= c& c =d& d=e & e>f"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(do.call(rbind, x$hyp_mat))), c(1, -1,  0,  0,  0,  0, 0,
                                                             0,  1, -1,  0,  0,  0, 0,
                                                             0,  0,  1, -1,  0,  0, 0,
                                                             0,  0,  0,  1, -1,  0, 0,
                                                             0,  0,  0,  0,  1, -1, 0))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(4,1))})

# HYPOTHESES SPECIFIED USING (,)
# 1) STRAIGHTFORWARD
# 2) WITH INAPPROPRIATE USED OF () RENDERING THE CORRECT RESULTS
# 3) LIKE 2)
# 4) A SERIES OF COMBINATIONS BETWEEN THE ()

varnames <- c("a","b","c","d","e","f")
hyp1 <- "(a-b,2*d -12) > (2* e + 2*f, c-9)"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(do.call(rbind, x$hyp_mat))), c(1, -1,  0, 0, -2, -2,  0,
                                                             0,  0,  0, 2, -2, -2, 12,
                                                             1, -1, -1, 0,  0,  0, -9,
                                                             0,  0, -1, 2,  0,  0,  3))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0,4))})

varnames <- c("a","b","c","d","e","f")
hyp1 <- "(b-a)>(c-d)>(d-f,e-f)"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(do.call(rbind, x$hyp_mat))), c(-1, 1, -1,  1,  0, 0, 0,
                                                             0, 0,  1, -2,  0, 1, 0,
                                                             0, 0,  1, -1, -1, 1, 0))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0,3))})

varnames <- c("a","b","c","d","e","f")
hyp1 <- "(2a -3b) > (4f-c)"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(do.call(rbind, x$hyp_mat))), c(2, -3, 1, 0, 0, -4, 0))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0,1))})

varnames <- c("a","b","c","d","e","f")
hyp1 <- "(a,b,c,d) > (e,f); (a-d, a-e, a-f) > (b-d, d-e, b-f) = (c-f,c-e,c-d) < (a+b, c+d, e+f) & (a,b,c,d) = (e+2,2*f-0.5)"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(do.call(rbind, x$hyp_mat))), c(1,0,0,0 ,-1,0,0.0,
                                                             0,1,0,0 ,-1,0,0.0,
                                                             0,0,1,0, -1,0,0.0,
                                                             0,0,0,1 ,-1,0,0.0,
                                                             1,0,0,0,0, -1,0.0,
                                                             0,1,0,0,0, -1,0.0,
                                                             0,0,1,0,0, -1,0.0,
                                                             0,0,0,1,0, -1,0.0,
                                                             0,1 ,-1, -1,0,1,0.0,
                                                             0,0 ,-1,1, -1,1,0.0,
                                                             0,1, -1,0,0,0,0.0,
                                                             0,1, -1 ,-1,1,0,0.0,
                                                             0,0, -1,1,0,0,0.0,
                                                             0,1, -1,0,1, -1,0.0,
                                                             0,1, -1,0,0,0,0.0,
                                                             0,0, -1,2, -1,0,0.0,
                                                             0,1 ,-1,1,0 ,-1,0.0,
                                                             1,0,0,0, -1,0,2.0,
                                                             0,1,0,0, -1,0,2.0,
                                                             0,0,1,0, -1,0,2.0,
                                                             0,0,0,1, -1,0,2.0,
                                                             1,0,0,0,0 ,-2, -0.5,
                                                             0,1,0,0,0 ,-2, -0.5,
                                                             0,0,1,0,0, -2, -0.5,
                                                             0,0,0,1,0, -2, -0.5,
                                                             1 ,-1,0,0,0,0,0.0,
                                                             1, -1,0,1 ,-1,0,0.0,
                                                             1, -1,0,1,0, -1,0.0,
                                                             1,0,0, -2,1,0,0.0,
                                                             1,0,0, -1,0,0,0.0,
                                                             1,0,0, -1,1, -1,0.0,
                                                             1 ,-1,0, -1,0,1,0.0,
                                                             1 ,-1,0,0 ,-1,1,0.0,
                                                             1, -1,0,0,0,0,0.0,
                                                             1,1 ,-1,0,0,1,0.0,
                                                             1,1 ,-1,0,1,0,0.0,
                                                             1,1, -1,1,0,0,0.0,
                                                             0,0,0,1,0,1,0.0,
                                                             0,0,0,1,1,0,0.0,
                                                             0,0,0,2,0,0,0.0,
                                                             0,0, -1,0,1,2,0.0,
                                                             0,0, -1,0,2,1,0.0,
                                                             0,0 ,-1,1,1,1,0.0))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0,8,17,18))})
