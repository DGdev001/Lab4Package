library(testthat)

test_that("myncurve returns a list", {
  result <- myncurve(mu = 0, sigma = 1, a = 1)
  expect_type(result, "list")
})

test_that("myncurve returns correct parameters", {
  result <- myncurve(mu = 5, sigma = 2, a = 3)
  expect_equal(result$mu, 5)
  expect_equal(result$sigma, 2)
  expect_equal(result$a, 3)
})

test_that("myncurve calculates correct probability", {
  mu <- 0
  sigma <- 1
  a <- 1.5
  result <- myncurve(mu, sigma, a)
  expected_prob <- round(pnorm(a, mean = mu, sd = sigma), 4)
  expect_equal(result$probability, expected_prob)
})
