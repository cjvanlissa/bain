# ===========================================================
# T-TESTS
# ===========================================================

# ===========================================================
# ONE GROUP
# ===========================================================

d1 <- as.data.frame(cbind(sesamesim$postnumb))
names(d1)<-c("postnumb")

for (tel in 1:5){
bainAnalysis <-bain:::bain_ttest_cran(x=d1$postnumb,
                               nu=30,type=tel,seed=900)

if (tel == 1) {
  set.seed(900)
  tt <- t_test(d1$postnumb)
  ttout <-  bain(tt,"x=30")
  
  # COMPARING THE WRAPPER FOR JASP WITH BAIN
  test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Fit , ttout$fit$Fit)})
  test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Com , ttout$fit$Com)})
  test_that("Bain mutual", {expect_equal(bainAnalysis$b, ttout$b)})
  test_that("Bain mutual", {expect_equal(as.vector(bainAnalysis$posterior), as.vector(ttout$posterior))})
  test_that("Bain mutual", {expect_equal(bainAnalysis$fit$BF,ttout$fit$BF)})
  test_that("Bain mutual", {expect_equal(bainAnalysis$fit$PMPb , ttout$fit$PMPb)})
  test_that("Bain mutual", {expect_equal(as.vector(t(bainAnalysis$BFmatrix)), as.vector(t(ttout$BFmatrix)))})
  test_that("Bain mutual", {expect_equal(summary(bainAnalysis),summary(ttout))})
  
  # # COMPARING JASP WITH THE WRAPPER FOR JASP  
  # BF_0u <- bainAnalysis$fit$BF[1]
  # PMP_u <- bainAnalysis$fit$PMPb[2]
  # PMP_0 <- bainAnalysis$fit$PMPb[1]
  # print(c(BF_0u,PMP_u,PMP_0))
  }

if (tel == 2) {
  set.seed(900)
  tt <- t_test(d1$postnumb)
  ttout <-  bain(tt,"x=30;x>30")
  
  # COMPARING THE WRAPPER FOR JASP WITH BAIN
  test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Fit , ttout$fit$Fit)})
  test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Com , ttout$fit$Com)})
  test_that("Bain mutual", {expect_equal(bainAnalysis$b, ttout$b)})
  test_that("Bain mutual", {expect_equal(as.vector(bainAnalysis$posterior), as.vector(ttout$posterior))})
  test_that("Bain mutual", {expect_equal(bainAnalysis$fit$BF,ttout$fit$BF)})
  test_that("Bain mutual", {expect_equal(bainAnalysis$fit$PMPb , ttout$fit$PMPb)})
  test_that("Bain mutual", {expect_equal(as.vector(t(bainAnalysis$BFmatrix)), as.vector(t(ttout$BFmatrix)))})
  test_that("Bain mutual", {expect_equal(summary(bainAnalysis),summary(ttout))})
  
  # # COMPARING JASP WITH THE WRAPPER FOR JASP  
  # BF_01 <- bainAnalysis$BFmatrix[1,2]
  # PMP_1 <- bainAnalysis$fit$PMPa[2]
  # PMP_0 <- bainAnalysis$fit$PMPa[1]
  # print(c(BF_01,PMP_1,PMP_0))
  }

if (tel == 3) {
  set.seed(900)
  tt <- t_test(d1$postnumb)
  ttout <-  bain(tt,"x=30;x<30")
  
  # COMPARING THE WRAPPER FOR JASP WITH BAIN
  test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Fit , ttout$fit$Fit)})
  test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Com , ttout$fit$Com)})
  test_that("Bain mutual", {expect_equal(bainAnalysis$b, ttout$b)})
  test_that("Bain mutual", {expect_equal(as.vector(bainAnalysis$posterior), as.vector(ttout$posterior))})
  test_that("Bain mutual", {expect_equal(bainAnalysis$fit$BF,ttout$fit$BF)})
  test_that("Bain mutual", {expect_equal(bainAnalysis$fit$PMPb , ttout$fit$PMPb)})
  test_that("Bain mutual", {expect_equal(as.vector(t(bainAnalysis$BFmatrix)), as.vector(t(ttout$BFmatrix)))})
  test_that("Bain mutual", {expect_equal(summary(bainAnalysis),summary(ttout))})
  
  # # COMPARING JASP WITH THE WRAPPER FOR JASP  
  # BF_01 <- bainAnalysis$BFmatrix[1,2]
  # PMP_0 <- bainAnalysis$fit$PMPa[1]
  # PMP_1 <- bainAnalysis$fit$PMPa[2]
  # print(c(BF_01,PMP_0,PMP_1))
  }

if (tel == 4) {
  set.seed(900)
  tt <- t_test(d1$postnumb)
  ttout <-  bain(tt,"x>30;x<30")
  
  # COMPARING THE WRAPPER FOR JASP WITH BAIN
  test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Fit , ttout$fit$Fit)})
  test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Com , ttout$fit$Com)})
  test_that("Bain mutual", {expect_equal(bainAnalysis$b, ttout$b)})
  test_that("Bain mutual", {expect_equal(as.vector(bainAnalysis$posterior), as.vector(ttout$posterior))})
  test_that("Bain mutual", {expect_equal(bainAnalysis$fit$BF,ttout$fit$BF)})
  test_that("Bain mutual", {expect_equal(bainAnalysis$fit$PMPb , ttout$fit$PMPb)})
  test_that("Bain mutual", {expect_equal(as.vector(t(bainAnalysis$BFmatrix)), as.vector(t(ttout$BFmatrix)))})
  test_that("Bain mutual", {expect_equal(summary(bainAnalysis),summary(ttout))})
  
  # # COMPARING JASP WITH THE WRAPPER FOR JASP  
  # BF_01 <- bainAnalysis$BFmatrix[1,2]
  # PMP_0 <- bainAnalysis$fit$PMPa[1]
  # PMP_1 <- bainAnalysis$fit$PMPa[2]
  # print(c(BF_01,PMP_0,PMP_1))
  }

if (tel == 5) {
  set.seed(900)
  tt <- t_test(d1$postnumb)
  ttout <-  bain(tt,"x=30;x>30;x<30")
  
  # COMPARING THE WRAPPER FOR JASP WITH BAIN
  test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Fit , ttout$fit$Fit)})
  test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Com , ttout$fit$Com)})
  test_that("Bain mutual", {expect_equal(bainAnalysis$b, ttout$b)})
  test_that("Bain mutual", {expect_equal(as.vector(bainAnalysis$posterior), as.vector(ttout$posterior))})
  test_that("Bain mutual", {expect_equal(bainAnalysis$fit$BF,ttout$fit$BF)})
  test_that("Bain mutual", {expect_equal(bainAnalysis$fit$PMPb , ttout$fit$PMPb)})
  test_that("Bain mutual", {expect_equal(as.vector(t(bainAnalysis$BFmatrix)), as.vector(t(ttout$BFmatrix)))})
  test_that("Bain mutual", {expect_equal(summary(bainAnalysis),summary(ttout))})
  
  # # COMPARING JASP WITH THE WRAPPER FOR JASP  
  # BF_01 <- bainAnalysis$BFmatrix[1,2]
  # BF_02 <- bainAnalysis$BFmatrix[1,3]
  # BF_12 <- bainAnalysis$BFmatrix[2,3]
  # PMP_0 <- bainAnalysis$fit$PMPa[1]
  # PMP_1 <- bainAnalysis$fit$PMPa[2]
  # PMP_2 <- bainAnalysis$fit$PMPa[3]
  # print(c(BF_01,BF_02,BF_12,PMP_0,PMP_1,PMP_2))
  }
}

# ========================================================
# INDEPENDENT GROUPS
# ========================================================

d2 <- as.data.frame(cbind(sesamesim$postnumb,sesamesim$sex))
names(d2)<-c("postnumb","sex")
x<-d2$postnumb[which(d2$sex==1)]
y<-d2$postnumb[which(d2$sex==2)]

for (tel in 1:5){
  bainAnalysis <-bain:::bain_ttest_cran(x=x,y=y,type=tel,seed=900)
  
  if (tel == 1) {
    set.seed(900)
    tt <- t_test(x,y,paired = FALSE, var.equal = FALSE)
    ttout <-  bain(tt,"x=y")
    
    # COMPARING THE WRAPPER FOR JASP WITH BAIN
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Fit , ttout$fit$Fit)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Com , ttout$fit$Com)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$b, ttout$b)})
    test_that("Bain mutual", {expect_equal(as.vector(bainAnalysis$posterior), as.vector(ttout$posterior))})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$BF,ttout$fit$BF)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$PMPb , ttout$fit$PMPb)})
    test_that("Bain mutual", {expect_equal(as.vector(t(bainAnalysis$BFmatrix)), as.vector(t(ttout$BFmatrix)))})
    test_that("Bain mutual", {expect_equal(summary(bainAnalysis),summary(ttout))})
    
    # # COMPARING JASP WITH THE WRAPPER FOR JASP  
    # BF_0u <- bainAnalysis$fit$BF[1]
    # PMP_u <- bainAnalysis$fit$PMPb[2]
    # PMP_0 <- bainAnalysis$fit$PMPb[1]
    # print(c(BF_0u,PMP_u,PMP_0))
    }
  
  if (tel == 2) {
    set.seed(900)
    tt <- t_test(x,y,paired = FALSE, var.equal = FALSE)
    ttout <-  bain(tt,"x=y;x>y")
    
    # COMPARING THE WRAPPER FOR JASP WITH BAIN
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Fit , ttout$fit$Fit)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Com , ttout$fit$Com)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$b, ttout$b)})
    test_that("Bain mutual", {expect_equal(as.vector(bainAnalysis$posterior), as.vector(ttout$posterior))})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$BF,ttout$fit$BF)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$PMPb , ttout$fit$PMPb)})
    test_that("Bain mutual", {expect_equal(as.vector(t(bainAnalysis$BFmatrix)), as.vector(t(ttout$BFmatrix)))})
    test_that("Bain mutual", {expect_equal(summary(bainAnalysis),summary(ttout))})
    
    # # COMPARING JASP WITH THE WRAPPER FOR JASP  
    # BF_01 <- bainAnalysis$BFmatrix[1,2]
    # PMP_1 <- bainAnalysis$fit$PMPa[2]
    # PMP_0 <- bainAnalysis$fit$PMPa[1]
    # print(c(BF_01,PMP_1,PMP_0))
    }
  
  if (tel == 3) {
    set.seed(900)
    tt <- t_test(x,y,paired = FALSE, var.equal = FALSE)
    ttout <-  bain(tt,"x=y;x<y")
    
    # COMPARING THE WRAPPER FOR JASP WITH BAIN
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Fit , ttout$fit$Fit)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Com , ttout$fit$Com)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$b, ttout$b)})
    test_that("Bain mutual", {expect_equal(as.vector(bainAnalysis$posterior), as.vector(ttout$posterior))})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$BF,ttout$fit$BF)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$PMPb , ttout$fit$PMPb)})
    test_that("Bain mutual", {expect_equal(as.vector(t(bainAnalysis$BFmatrix)), as.vector(t(ttout$BFmatrix)))})
    test_that("Bain mutual", {expect_equal(summary(bainAnalysis),summary(ttout))})
    
    # # COMPARING JASP WITH THE WRAPPER FOR JASP  
    # BF_01 <- bainAnalysis$BFmatrix[1,2]
    # PMP_0 <- bainAnalysis$fit$PMPa[1]
    # PMP_1 <- bainAnalysis$fit$PMPa[2]
    # print(c(BF_01,PMP_0,PMP_1))
    }
  
  if (tel == 4) {
    set.seed(900)
    tt <- t_test(x,y,paired = FALSE, var.equal = FALSE)
    ttout <-  bain(tt,"x>y;x<y")
    
    # COMPARING THE WRAPPER FOR JASP WITH BAIN
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Fit , ttout$fit$Fit)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Com , ttout$fit$Com)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$b, ttout$b)})
    test_that("Bain mutual", {expect_equal(as.vector(bainAnalysis$posterior), as.vector(ttout$posterior))})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$BF,ttout$fit$BF)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$PMPb , ttout$fit$PMPb)})
    test_that("Bain mutual", {expect_equal(as.vector(t(bainAnalysis$BFmatrix)), as.vector(t(ttout$BFmatrix)))})
    test_that("Bain mutual", {expect_equal(summary(bainAnalysis),summary(ttout))})
    
    # # COMPARING JASP WITH THE WRAPPER FOR JASP  
    # BF_01 <- bainAnalysis$BFmatrix[1,2]
    # PMP_0 <- bainAnalysis$fit$PMPa[1]
    # PMP_1 <- bainAnalysis$fit$PMPa[2]
    # print(c(BF_01,PMP_0,PMP_1))
    }
  
  if (tel == 5) {
    set.seed(900)
    tt <- t_test(x,y,paired = FALSE, var.equal = FALSE)
    ttout <-  bain(tt,"x=y;x>y;x<y")
    
    # COMPARING THE WRAPPER FOR JASP WITH BAIN
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Fit , ttout$fit$Fit)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Com , ttout$fit$Com)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$b, ttout$b)})
    test_that("Bain mutual", {expect_equal(as.vector(bainAnalysis$posterior), as.vector(ttout$posterior))})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$BF,ttout$fit$BF)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$PMPb , ttout$fit$PMPb)})
    test_that("Bain mutual", {expect_equal(as.vector(t(bainAnalysis$BFmatrix)), as.vector(t(ttout$BFmatrix)))})
    test_that("Bain mutual", {expect_equal(summary(bainAnalysis),summary(ttout))})
    
    # # COMPARING JASP WITH THE WRAPPER FOR JASP  
    # BF_01 <- bainAnalysis$BFmatrix[1,2]
    # BF_02 <- bainAnalysis$BFmatrix[1,3]
    # BF_12 <- bainAnalysis$BFmatrix[2,3]
    # PMP_0 <- bainAnalysis$fit$PMPa[1]
    # PMP_1 <- bainAnalysis$fit$PMPa[2]
    # PMP_2 <- bainAnalysis$fit$PMPa[3]
    # print(c(BF_01,BF_02,BF_12,PMP_0,PMP_1,PMP_2))
    }
}

# ========================================================
# PAIRED SAMPLES
# ========================================================

d3 <- as.data.frame(cbind(sesamesim$postnumb,sesamesim$prenumb))
names(d3)<-c("postnumb","prenumb")
x <- d3$postnumb
y <- d3$prenumb

for (tel in 1:5){
  bainAnalysis <-bain:::bain_ttest_cran(x=x,y=y,type=tel,paired=TRUE,seed=900)
  
  if (tel == 1) {
    set.seed(900)
    tt <- t_test(x,y,paired = TRUE)
    ttout <-  bain(tt,"difference=0")
    
    # COMPARING THE WRAPPER FOR JASP WITH BAIN
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Fit , ttout$fit$Fit)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Com , ttout$fit$Com)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$b, ttout$b)})
    test_that("Bain mutual", {expect_equal(as.vector(bainAnalysis$posterior), as.vector(ttout$posterior))})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$BF,ttout$fit$BF)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$PMPb , ttout$fit$PMPb)})
    test_that("Bain mutual", {expect_equal(as.vector(t(bainAnalysis$BFmatrix)), as.vector(t(ttout$BFmatrix)))})
    test_that("Bain mutual", {expect_equal(summary(bainAnalysis),summary(ttout))})
    
    # # COMPARING JASP WITH THE WRAPPER FOR JASP  
    # BF_0u <- bainAnalysis$fit$BF[1]
    # PMP_u <- bainAnalysis$fit$PMPb[2]
    # PMP_0 <- bainAnalysis$fit$PMPb[1]
    # print(c(BF_0u,PMP_u,PMP_0))
    }
  
  if (tel == 2) {
    set.seed(900)
    tt <- t_test(x,y,paired = TRUE)
    ttout <-  bain(tt,"difference=0;difference>0")
    
    # COMPARING THE WRAPPER FOR JASP WITH BAIN
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Fit , ttout$fit$Fit)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Com , ttout$fit$Com)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$b, ttout$b)})
    test_that("Bain mutual", {expect_equal(as.vector(bainAnalysis$posterior), as.vector(ttout$posterior))})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$BF,ttout$fit$BF)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$PMPb , ttout$fit$PMPb)})
    test_that("Bain mutual", {expect_equal(as.vector(t(bainAnalysis$BFmatrix)), as.vector(t(ttout$BFmatrix)))})
    test_that("Bain mutual", {expect_equal(summary(bainAnalysis),summary(ttout))})
    
    # # COMPARING JASP WITH THE WRAPPER FOR JASP  
    # BF_01 <- bainAnalysis$BFmatrix[1,2]
    # PMP_1 <- bainAnalysis$fit$PMPa[2]
    # PMP_0 <- bainAnalysis$fit$PMPa[1]
    # print(c(BF_01,PMP_1,PMP_0))
    }
  
  if (tel == 3) {
    set.seed(900)
    tt <- t_test(x,y,paired = TRUE)
    ttout <-  bain(tt,"difference=0;difference<0")
    
    # COMPARING THE WRAPPER FOR JASP WITH BAIN
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Fit , ttout$fit$Fit)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Com , ttout$fit$Com)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$b, ttout$b)})
    test_that("Bain mutual", {expect_equal(as.vector(bainAnalysis$posterior), as.vector(ttout$posterior))})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$BF,ttout$fit$BF)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$PMPb , ttout$fit$PMPb)})
    test_that("Bain mutual", {expect_equal(as.vector(t(bainAnalysis$BFmatrix)), as.vector(t(ttout$BFmatrix)))})
    test_that("Bain mutual", {expect_equal(summary(bainAnalysis),summary(ttout))})
    
    # # COMPARING JASP WITH THE WRAPPER FOR JASP  
    # BF_01 <- bainAnalysis$BFmatrix[1,2]
    # PMP_0 <- bainAnalysis$fit$PMPa[1]
    # PMP_1 <- bainAnalysis$fit$PMPa[2]
    # print(c(BF_01,PMP_0,PMP_1))
    }
  
  if (tel == 4) {
    set.seed(900)
    tt <- t_test(x,y,paired = TRUE)
    ttout <-  bain(tt,"difference>0;difference<0")
    
    # COMPARING THE WRAPPER FOR JASP WITH BAIN
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Fit , ttout$fit$Fit)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Com , ttout$fit$Com)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$b, ttout$b)})
    test_that("Bain mutual", {expect_equal(as.vector(bainAnalysis$posterior), as.vector(ttout$posterior))})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$BF,ttout$fit$BF)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$PMPb , ttout$fit$PMPb)})
    test_that("Bain mutual", {expect_equal(as.vector(t(bainAnalysis$BFmatrix)), as.vector(t(ttout$BFmatrix)))})
    test_that("Bain mutual", {expect_equal(summary(bainAnalysis),summary(ttout))})
    
    # # COMPARING JASP WITH THE WRAPPER FOR JASP  
    # BF_01 <- bainAnalysis$BFmatrix[1,2]
    # PMP_0 <- bainAnalysis$fit$PMPa[1]
    # PMP_1 <- bainAnalysis$fit$PMPa[2]
    # print(c(BF_01,PMP_0,PMP_1))
    }
  
  if (tel == 5) {
    set.seed(900)
    tt <- t_test(x,y,paired = TRUE)
    ttout <-  bain(tt,"difference=0;difference>0;difference<0")
    
    # COMPARING THE WRAPPER FOR JASP WITH BAIN
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Fit , ttout$fit$Fit)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$Com , ttout$fit$Com)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$b, ttout$b)})
    test_that("Bain mutual", {expect_equal(as.vector(bainAnalysis$posterior), as.vector(ttout$posterior))})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$BF,ttout$fit$BF)})
    test_that("Bain mutual", {expect_equal(bainAnalysis$fit$PMPb , ttout$fit$PMPb)})
    test_that("Bain mutual", {expect_equal(as.vector(t(bainAnalysis$BFmatrix)), as.vector(t(ttout$BFmatrix)))})
    test_that("Bain mutual", {expect_equal(summary(bainAnalysis),summary(ttout))})
    
    # # COMPARING JASP WITH THE WRAPPER FOR JASP  
    # BF_01 <- bainAnalysis$BFmatrix[1,2]
    # BF_02 <- bainAnalysis$BFmatrix[1,3]
    # BF_12 <- bainAnalysis$BFmatrix[2,3]
    # PMP_0 <- bainAnalysis$fit$PMPa[1]
    # PMP_1 <- bainAnalysis$fit$PMPa[2]
    # PMP_2 <- bainAnalysis$fit$PMPa[3]
    # print(c(BF_01,BF_02,BF_12,PMP_0,PMP_1,PMP_2))
    }
}




