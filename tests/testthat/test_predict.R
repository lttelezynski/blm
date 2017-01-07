context("blm-predict")

test_that("testing predict function", {
  # Put some initial tests here.
  x <- stats::rnorm(100, 10, 5)
  z<- stats::rnorm(100, 5, 5)
  a <- 1
  b <- 2
  c<-3
  y <- a+b*x

  fit <- blm(y~x,1,1)

  expect_equal(predict(fit), y, tolerance=0.1)
})