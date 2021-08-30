
varnames<-c("a1","b1","a2","b2")
hypotheses31 <-
  "0.20 *a1 = 0.20 *a2&
0.20 *b1 = 0.20 *b2 &"
x1<-bain:::parse_hypothesis(varnames, hypotheses31)

varnames<-c("a1","b1","a2","b2")
hypotheses31 <-
  ".20 *a1 = .20 *a2&
.20 *b1 = .20 *b2 &";
x2<-bain:::parse_hypothesis(varnames, hypotheses31)

test_that("parser", {expect_equal(as.vector(t(do.call(rbind, x1$hyp_mat))),
                                  as.vector(t(do.call(rbind, x2$hyp_mat))))})


varnames<-c("a1","b1","c1","d1","e1","f1","a2","b2","c2","d2","e2","f2")
hypotheses31 <-
  "a1 = a2 - 0.2&
b1 = b2 + .2 &
0.2*c1 -.3 = c2 +.3&
d1 + .3= 3*d2 + .3&
e1 -.3= e2 + .3&
4*f1 -0.3 = 3.2*f2"
x3<-bain:::parse_hypothesis(varnames, hypotheses31)

test_that("parser", {expect_equal(as.vector(t(do.call(rbind, x3$hyp_mat))),
                                  c(1 , 0, 0.0 , 0, 0 , 0 ,-1 ,0 , 0 , 0, 0 , 0.0, -0.2,
                                    0 , 1, 0.0, 0, 0 , 0 ,0, -1 ,0 ,0 , 0, 0.0, 0.2,
                                    0 , 0, 0.2, 0, 0 , 0 , 0 ,0, -1, 0 , 0 , 0.0 , 0.6,
                                    0 ,0, 0.0 , 1 , 0, 0 , 0 ,0 , 0, -3, 0 ,0.0 , 0.0,
                                    0, 0, 0.0, 0, 1 , 0, 0, 0 , 0, 0 ,-1 , 0.0 , 0.6,
                                    0, 0, 0.0, 0, 0, 4, 0 , 0, 0, 0 , 0 ,-3.2, 0.3)
)})
