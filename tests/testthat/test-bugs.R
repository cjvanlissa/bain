# bij de onderstaande ancova werk label_estimates niet, dwz, de namen van de covariaten worden
# niet hernoemd

sesamesim$sex <- as.factor(sesamesim$sex)
ancov <- lm(postnumb ~ sex + prenumb + peabody -1, data = sesamesim)
coef(ancov)
ancov <- label_estimates(ancov, c("a", "b","pre", "pea"))
set.seed(100)
z<-bain(ancov, " pre > 0 &  pea > 0")


sesamesim$site <- as.factor(sesamesim$site)
sesamesim$prenumb <- sesamesim$prenumb - mean(sesamesim$prenumb)
ancov <- lm(postnumb~site+prenumb-1,sesamesim)
coef(ancov)
ancov <- label_estimates(ancov, c("site1", "site2", "site3","site4","site5","pre"))
set.seed(100)
results <- bain(ancov, "site1=site2=site3=site4=site5; site2>site5>site1>site3>site4")

# de standardize regression werkt niet
regr <- lm(postnumb ~ prenumb + peabody, sesamesim)
get_estimates(regr)
regr <- label_estimates(regr, c("i", "num", "pea"))
set.seed(100)
z<-bain(regr,"num=pea", standardize = TRUE)

# de t.test met formule invoer, dwz, postnumb~sex, werkt de label estimates niet en mogelijk bain ook niet
# This is because bain overwrites only t.test.default. t.test.formula calls t.test, which is found within the package environment
sesamesim$sex<-as.factor(sesamesim$sex)
#ttest <- t.test(sesamesim$postnumb[sesamesim$sex==1],sesamesim$postnumb[sesamesim$sex==2],paired = FALSE, var.equal = FALSE)
ttest <- t.test(postnumb~sex,data=sesamesim,paired = FALSE, var.equal = FALSE)
expect_error(ttest <- label_estimates(ttest, c("m1","m2")))
set.seed(100)
expect_error(z <- bain(ttest, "m1=m2; m1>m2; m1<m2"))

#===========================================================================
# DESCRIPTIVE THE T-TEST
# - BUG1 DE SAMPLE SIZES WORDEN VERKEERD WEERGEGEVEN
# - BUG2 DE PARAMETER CI HEEFT GEEN EFFECT, .95 EN .99 LEIDEN TOT DEZELFDE LB EN UB

# DE SAMPLE SIZES WORDEN EIGENLIJK OVERAL (TTEST, ANOVA, ANCOVA) FOUT WEERGEGEVEN. HET IS OOK LASTIG OM DE
# SAMPLE SIZES PER PARAMETER TE GEVEN/INTERPRETEREN. HET IS BEDENK IK NU BETER OM ZE ONDER DE TABEL
# TOE TE VOEGEN. DAT WIL ZEGGEN PRINT OUTOBJECT$n MET DAARAAN VOORAFGAAND DE TEKST
# "THE SAMPLE SIZES PER GROUP" ALS LAATSTE REGEL IN DE TABEL.
#===========================================================================

rm(list=ls())

x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]
ttest <- t.test(x,y,paired = FALSE, var.equal = TRUE)
ttest <- label_estimates(ttest, c("boy","girl"))
set.seed(100)
results <- bain(ttest, "boy = girl; boy > girl; boy < girl")
des1 <- descriptives(results, ci = 0.95)

test_that("descriptives", {expect_equal(des1$Estimate , as.numeric(results$estimates))})
test_that("descriptives", {expect_equal(des1$n , as.numeric(results$n))})
lbd <- as.vector(results$estimates + qnorm(.025) * c(sqrt(results$posterior[1,1]),sqrt(results$posterior[2,2])))
test_that("descriptives", {expect_equal(des1$lb , lbd)})
ubd <- as.vector(results$estimates + qnorm(.975) * c(sqrt(results$posterior[1,1]),sqrt(results$posterior[2,2])))
test_that("descriptives", {expect_equal(des1$ub , ubd)})

des2 <- descriptives(results, ci = 0.98)

test_that("descriptives", {expect_equal(des2$Estimate , as.numeric(results$estimates))})
test_that("descriptives", {expect_equal(des2$n , as.numeric(results$n))})
lbd <- as.vector(results$estimates + qnorm(.01) * c(sqrt(results$posterior[1,1]),sqrt(results$posterior[2,2])))
test_that("descriptives", {expect_equal(des2$lb , lbd)})
ubd <- as.vector(results$estimates + qnorm(.99) * c(sqrt(results$posterior[1,1]),sqrt(results$posterior[2,2])))
test_that("descriptives", {expect_equal(des2$ub , ubd)})

#===========================================================================
# DESCRIPTIVES THE ONE-SAMPLE T-TEST - HET DECRIPTIVES COMMANDO GEEFT EEN FOUTMELDING (idem voor paired t-test)
#===========================================================================

rm(list=ls())

ttest <- t.test(sesamesim$postnumb)
ttest <- label_estimates(ttest, c("post"))
set.seed(100)
results <- bain(ttest, "post=30; post>30; post<30")
des1 <- descriptives(results, ci = 0.95)

