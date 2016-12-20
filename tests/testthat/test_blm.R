context("blm")

test_that("we can fit blm objects", {
  # Put some initial tests here.
  x <- stats::rnorm(100, 10, 5)
  z<- stats::rnorm(100, 5, 5)
  a <- 1
  b <- 2
  c<-3
  y <- a+b*x

  fit <- blm(y~x,1,1)
  cof<-coefficients(fit)
  names(cof)<-NULL
  expect_equal(cof[1], a, tolerance=0.1)
  expect_equal(cof[2], b, tolerance=0.1)
})