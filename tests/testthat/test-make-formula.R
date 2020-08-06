context("test-make-formula.R")

test_that("make.formula works", {

  model <- as.formula("y1 ~ x1 + x2 + x3",  env = parent.frame())

  vars.temp <- paste0("x", 1:3)
  y <- paste0("y", 1)
  out <- make.formula(y, vars.temp)

  expect_equal(out, model)
})
