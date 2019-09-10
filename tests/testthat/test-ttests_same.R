# Welch two-sample
tt <- t_test(x = c(1:10), c(7:20))
tt_old <- bain:::t_test_old(x = c(1:10), y = c(7:20))
test_that("Welch two-sample equal vcov", {expect_equal(tt$v, tt_old$v)})
tt$v
tt_old$v
tt$n
tt_old$n
# Welch two-sample sesamesim
tt <- t_test(sesamesim$peabody[sesamesim$sex == 1], sesamesim$peabody[sesamesim$sex == 2])
tt_old <- bain:::t_test_old(sesamesim$peabody[sesamesim$sex == 1], sesamesim$peabody[sesamesim$sex == 2])
test_that("Welch two-sample equal vcov", {expect_equal(tt$v, tt_old$v)})


# Welch two-sample formula interface
data(sesamesim)
tt <- t_test(peabody ~ sex, data = sesamesim)
tt_old <- bain:::t_test_old(peabody ~ sex, data = sesamesim)
test_that("Two-sample equal variances equal vcov", {expect_equal(tt$v, tt_old$v)})

# Equal variance t-test
tt <- t_test(sesamesim$peabody)
tt_old <- bain:::t_test_old(sesamesim$peabody)
test_that("One-sample equal vcov", {expect_equal(tt$v, tt_old$v)})

# Paired samples
tt <- t_test(sesamesim$prenumb, sesamesim$postnumb, paired = TRUE)
tt_old <- bain:::t_test_old(sesamesim$prenumb, sesamesim$postnumb, paired = TRUE)
test_that("Paired samples equal vcov", {expect_equal(tt$v, tt_old$v)})

