context("bain methods")

estimate<-c(3,2,1)  #estimates of coefficients
Sigma<-matrix(c(3,0,0,0,2,0,0,0,1),3,3,byrow = TRUE) #covariance matrix of coefficients

n<-50 #samples size

#H1
ERr<-NULL
IRr<-matrix(c(1,-1,0,0,
              0,1,-1,0), ncol=4,byrow = TRUE)
bain_res <- bain(c(a=3, b=2, c=1), "a>b>c", n = n, Sigma = Sigma, groups = 0, joint_parameters = 3)

Bain_res <- bain:::Bain(estimate=estimate,grouppara=0,jointpara=3,Sigma=Sigma,n=n,ERr,IRr) #run

test_that("bain.default works", {
  expect_equal(unlist(bain_res$fit[1,]),
               c(Fit_eq = 1, Com_eq = 1, Fit_in = 0.425272985864483, Com_in = 0.164505790338879,
                   Fit = 0.425272985864483, Com = 0.164505790338879, BF = 3.758,
                   PMPa = 1, PMPb = 0.72107204094751), tolerance = .01)
})

test_that("bain.default returns the same as Bain", {
  bain_result <- unlist(unname(bain_res$fit[1, c(1,3,2,4:ncol(bain_res$fit))]))
  Bain_result <- as.numeric(as.character(unlist(Bain_res$fit_com_table[1,])))
  expect_equal(bain_result, Bain_result, tolerance = .01)
})
