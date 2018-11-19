context("parse_hypothesis")

test_that("scalar can follow varname", {
  expect_identical(as.vector(bain:::parse_hypothesis(varnames = "a", hyp = "a*30<1")$hyp_mat), c(-30, -1))
  })

