m_lm <- lm(iris[[1]]~iris[[5]]-1, iris)
get_estimates(m_lm)
new_labels <- c("a", "c", "b")
m_lm <- label_estimates(m_lm, new_labels)

context("label_estimates")

test_that("label_estimates does not result in error", {
  expect_error(bain(m_lm, "a < b < c"), NA)
})

test_that("label_estimates renames estimates", {
  expect_identical(names(m_lm$coefficients), new_labels)
})

test_that("label_estimates also renames factor levels", {
  expect_identical(levels(m_lm$model$`iris[[5]]`), new_labels)
})
