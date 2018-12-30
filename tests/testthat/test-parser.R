# TESTING THE BAIN PARSER
# ALTHOUGH MORE IS POSSIBLE, THE USER IS REQUIRED TO USE STICK TO THE FOLLOWING SYNTAX:
# 1) COMBINATIONS OF number * par + number FOR MULTIPLE PARAMETERS
# 2) USE & TO COMBINE DIFFERENT PARTS OF AN HYPOTHESIS
# 3) USE (,) TO CREATE SET OF COMBINATIONS
# BELOW ONLY THESE OPTIONS ARE DEBUGGED, AT THE END OF THIS FILE OTHER TEST FOR 
# SYNTAX THAT MAY OR MAY NOT WORK IS COMMENTED OUT

library(bain)
library(testthat)

# TESTING THE NAMING OF THE PARAMETERS
# 1) USE OF LETTERS, . AND _
# 2) USE OF CAPITALS AND NUMBERS

varnames <- c("a._", "b_.", "c.c_")
hyp1 <- "a._ > b_. > c.c_"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(x$hyp_mat)), c(1,-1,0,0,0,1,-1,0))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0,2))})

varnames <- c("a2b", "b2G", "C")
hyp1 <- "a2b > b2G > C"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(x$hyp_mat)), c(1,-1,0,0,0,1,-1,0))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0,2))})

# TESTING THE USE OF LINEAR COMBINATIONS
# 1) SCALARS CONSISTING OF ONE OR TWO NUMBERS
# 2) SCALARS UP TO THREE NUMBERS, STARTING WITH . AND 0, NEGATIVE NUMBERS
# 3) A SINGLE "LONG" HYPOTHESIS
# 4) TESTS PROVIDED BY CASPAR
# 5) SERIES OF =, <, > IN ONE HYPOTHESIS
# 6) A SIMPLE EXAMPLE

varnames <- c("a", "b", "c")
hyp1 <- "20<-21*a<4.5; a>b> c; 4a > 7; 4 + 7 > a - 4* b  + 8"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(x$hyp_mat)), c(-21,  0,  0 ,20.0,
                                                             21,  0,  0 ,-4.5,
                                                             1, -1,  0,  0.0,
                                                             0 , 1, -1,  0.0,
                                                             4,  0,  0,  7.0,
                                                             -1,  4 , 0, -3.0))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0, 2, 0, 2, 0, 1, 0, 1))})

varnames <- c("a22","b19")
hyp1 <- "12*a22 > 2; 243*b19 > .666; 5* a22 - 54*b19 < 45; -12*a22 > -2; 243*b19 < -0.666; 5* a22 - 54*b19 = 45"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(x$hyp_mat)), c(12,    0,   2.000,
                                                             0,  243 ,  0.666,
                                                             -5 ,  54 ,-45.000,
                                                             -12 ,   0 , -2.000,
                                                             0 ,-243 ,  0.666,
                                                             5 , -54,  45.000))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0))})

varnames <- c("a","b","c","d","e","f")
hyp1 <- "2* a -3*b -4*f + c > 2"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(x$hyp_mat)), c(2, -3, 1, 0, 0, -4, 2))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0,1))})

varnames <- c("a","b","coef1","coef2","coef_1")
hyp1<-"2=a;a=2; 2*a+2=4;2* a+4* b=5+7; 2*a=3+6*b; 2=coef1; coef1=2;2*coef1+2=4; 2*coef1+4*coef2=5+7;2=coef_1;coef_1=2"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(x$hyp_mat)), c(-1,  0,     0,     0 ,     0 ,-2,
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
test_that("parser", {expect_equal(as.vector(t(x$hyp_mat)), c(1, -1,  0,  0, 0,
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
test_that("parser", {expect_equal(as.vector(t(x$hyp_mat)), c(-2, 3, -1, 0, 0, 4, 0))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0,1))})

# hypotheses specified using the &
# 1) STRAIGHTFORWARD AND WITH ONE USE OF (,)
# 2) SIMPLE
# 3) WRONG INPUT CORRECT RESULT
# 4) A SERIES OF &

varnames <- c("a1","a2","a3")
hyp1 <- "a1 = a2 & a2 = a3; (a1,a2) > a3 & a1 =a2"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(x$hyp_mat)), c(1, -1,  0, 0,
                                                             0,  1, -1, 0,
                                                             1, -1,  0, 0,
                                                             1,  0 ,-1, 0,
                                                             0,  1, -1, 0))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(2, 0, 1, 2))})

varnames <- c("a","b","c","d","e","f")
hyp1 <- "a=b&c=d"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(x$hyp_mat)), c(0, 1, -1, 0, 0, 0, 0))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(1,0))})

varnames <- c("a","b","c","d","e","f")
hyp1 <- "a&b=c&d"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(x$hyp_mat)), c(0, 1, -1, 0, 0, 0, 0))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(1,0))})

varnames <- c("a","b","c","d","e","f")
hyp1 <- "a = b & b= c& c =d& d=e & e>f"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(x$hyp_mat)), c(1, -1,  0,  0,  0,  0, 0,
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
test_that("parser", {expect_equal(as.vector(t(x$hyp_mat)), c(1, -1,  0, 0, -2, -2,  0,
                                                             0,  0,  0, 2, -2, -2, 12,
                                                             1, -1, -1, 0,  0,  0, -9,
                                                             0,  0, -1, 2,  0,  0,  3))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0,4))})

varnames <- c("a","b","c","d","e","f")
hyp1 <- "(b-a)>(c-d)>(d-f,e-f)"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(x$hyp_mat)), c(-1, 1, -1,  1,  0, 0, 0,
                                                             0, 0,  1, -2,  0, 1, 0,
                                                             0, 0,  1, -1, -1, 1, 0))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0,3))})

varnames <- c("a","b","c","d","e","f")
hyp1 <- "(2a -3b) > (4f-c)"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(x$hyp_mat)), c(2, -3, 1, 0, 0, -4, 0))})
test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0,1))})

varnames <- c("a","b","c","d","e","f")
hyp1 <- "(a,b,c,d) > (e,f); (a-d, a-e, a-f) > (b-d, d-e, b-f) = (c-f,c-e,c-d) < (a+b, c+d, e+f) & (a,b,c,d) = (e+2,2*f-0.5)"
x<-bain:::parse_hypothesis(varnames, hyp1)
test_that("parser", {expect_equal(as.vector(t(x$hyp_mat)), c(1,0,0,0 ,-1,0,0.0,
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



# # testing multiplyer after parameter name
# 
# varnames = "a.2"
# hyp1 = "a.2*30>1"
# x<-bain:::parse_hypothesis(varnames, hyp1)
# test_that("parser", {expect_equal(as.vector(t(x$hyp_mat)), c(30,1))})
# test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0,1))})
# 
# varnames = "a.2"
# hyp1 = "a.2*30<1"
# x<-bain:::parse_hypothesis(varnames, hyp1)
# test_that("parser", {expect_equal(as.vector(t(x$hyp_mat)), c(-30,-1))})
# test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0,1))})
# 
# varnames = "a.2"
# hyp1 = "39*a.2>1"
# x<-bain:::parse_hypothesis(varnames, hyp1)
# test_that("parser", {expect_equal(as.vector(t(x$hyp_mat)), c(39,1))})
# test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0,1))})
# 
# varnames = "a.2"
# hyp1 = "333*a.2<1"
# x<-bain:::parse_hypothesis(varnames, hyp1)
# test_that("parser", {expect_equal(as.vector(t(x$hyp_mat)), c(-333,-1))})
# test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0,1))})
# 
# # met alleen multiplyers geplakt aan een parameter gaat het helemaal prima
# 
# varnames <- c("a","b", "c")
# hyp1 <- "4 * a - 3 * a + 3 * c + 5 - 2 * b + a + 8 * b + 7 = -2"
# x<-bain:::parse_hypothesis(varnames, hyp1)
# 
# # met complexe splitsingen gaat het ALLEMAAL NIET GOED ZOALS HIERONDER TE ZIEN VALT
# 
# varnames <- c("a","b", "c")
# hyp1 <- "2*1.5*a + .5 + .5 > 4*.5*b -1 + 1> c -1.5 -.5; 3*a + 1 > 2*b > c -2"
# x<-bain:::parse_hypothesis(varnames, hyp1)
# 
# # can two numbers be multiplied: YES VOOR DE TWEEDE, NO VOOR DE EERSTE
# 
# varnames <- c("boy","girl")
# hyp1 <- "boy - girl > -.1 * 2.52 & boy - girl < .1 * 2.52"
# x<-bain:::parse_hypothesis(varnames, hyp1)
# 
# varnames <- c("a","b")
# hyp1 <- "a-b > -.2 * 24 & a-b < .2 * 24"
# x<-bain:::parse_hypothesis(varnames, hyp1)
# # check use of double - - and + + signs. NOTE  the order - + does not work!
# 
# varnames <- c("a.1","b.1")
# hyp1 <- "a.1 - - b.1 = 8; a.1 + + b.1 = 8; a.1 + - b.1 = 8; a.1 - b.1< 8"
# x<-bain:::parse_hypothesis(varnames, hyp1)
# test_that("parser", {expect_equal(as.vector(t(x$hyp_mat)), c(1 ,  1,  8,
#                                                              1 ,  1 , 8,
#                                                              1 , -1,  8,
#                                                              -1,   1, -8))})
# test_that("parser", {expect_equal(as.vector(x$n_constraints), c(1, 0, 1, 0, 1, 0, 0, 1))})
#  
# # multiple occurances of the same parameter name 
# 
# varnames <- c("a_1","b", "c.1")
# hyp1 <- " a_1 > a_1 + b > c.1 -a_1 -b; (a_1+a_1+b+a_1,c.1+c.1+c.1)>b+b; b+b+b=0; (b+b+b) < 5"
# x<-bain:::parse_hypothesis(varnames, hyp1)
# test_that("parser", {expect_equal(as.vector(t(x$hyp_mat)), c(0, -1,   0,  0,
#                                                              2,  2,  -1,  0,
#                                                              3, -1,   0,  0,
#                                                              0, -2,   3,  0,
#                                                              0,  3,   0,  0,
#                                                              0, -3,   0, -5))})
# test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0, 2, 0, 2 ,1, 0, 0, 1))})
# # ====================================================================================================
# # BOUW IN JASP EN BAIN FOUTMELDING IN VOOR "gewoon gebruik van haakjes" en nonlinear constraints!!!!!
# # ====================================================================================================
# 
# varnames <- c("a","b","c","d","e","f")
# hyp1 <- "(a,b)>(c,d,e);(a,b)=(c,d,f); (a-b)-(c-d)=0"
# x<-bain:::parse_hypothesis(varnames, hyp1)
# test_that("parser", {expect_equal(as.vector(t(x$hyp_mat)), c(1,  0, -1,  0,  0,  0, 0,
#                                                              0,  1, -1,  0,  0,  0, 0,
#                                                              1,  0,  0, -1,  0,  0, 0,
#                                                              0,  1,  0, -1,  0,  0, 0,
#                                                              1,  0,  0,  0, -1,  0, 0,
#                                                              0,  1,  0,  0, -1,  0, 0,
#                                                              1,  0, -1,  0,  0,  0, 0,
#                                                              0,  1, -1,  0,  0,  0, 0,
#                                                              1,  0,  0, -1,  0,  0, 0,
#                                                              0,  1,  0, -1,  0,  0, 0,
#                                                              1,  0,  0,  0,  0, -1, 0,
#                                                              0,  1,  0,  0,  0, -1, 0,
#                                                              1, -1, -1,  1,  0,  0, 0))})
# test_that("parser", {expect_equal(as.vector(x$n_constraints), c(0, 6, 6, 0, 1, 0))})

