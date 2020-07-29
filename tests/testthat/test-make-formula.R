context("test-make-formula.R")

test_that("make.formula works", {

  model <- c("y1 ~ x1 + x2 + x3", "y2 ~ x1 + x2 + x3")

  vars.temp <- paste0("x", 1:3)
  y <- paste0("y", 1:2)
  out <- make.formula(y, vars.temp)

  expect_equal(out, noquote(model))
})
