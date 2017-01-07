context("blm")

test_that("testing blm objects", {
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

test_that("testing coefficients", {
  x <- stats::rnorm(1000, 10, 1)
  y<- stats::rnorm(1000, x, 1)


  fit <- blm(y~x,1,1)
  lmFit <- lm(y~x)
  co <- coefficients(fit)
  lmCo <- coef(lmFit)
  expect_equal(co, lmCo, tolerance=0.1)

})

test_that("testing ploting function", {
  # Put some initial tests here.
  x <- stats::rnorm(100, 10, 5)
  z<- stats::rnorm(100, 5, 5)
  a <- 1
  b <- 2
  c<-3
  y <- a+b*x

  fit <- blm(y~x,1,1)
  plot(fit1)

})

test_that("testing fitted", {
  x <- stats::rnorm(1000, 10, 1)
  y<- stats::rnorm(1000, x, 1)

  fit <- blm(y~x,1,1)
  lmFit <- lm(y~x)
  blmFitted <- blm::fitted(fit)
  lmFitted <- stats::fitted(lmFit)
  names(lmFitted)<-NULL
  expect_equal(blmFitted, lmFitted, tolerance=0.1)
})

test_that("testing residuals", {
  x <- stats::rnorm(1000, 10, 1)
  y<- stats::rnorm(1000, x, 1)

  fit <- blm(y~x,1,1)
  lmFit <- lm(y~x)
  blmRes <- blm::residuals(fit)
  lmRes <- stats::residuals(lmFit)
  names(lmRes)<-NULL
  expect_equal(blmRes, lmRes, tolerance=0.1)

})

test_that("testing deviance", {
  x <- stats::rnorm(1000, 10, 1)
  y<- stats::rnorm(1000, x, 1)
  fit1<-blm(y~x,1,1)
  blmRes <- deviance(fit1)
  lmFit<-lm(y~x)
  lmRes <- stats::deviance(lmFit)

  expect_equal(blmRes, lmRes, tolerance=0.1)
})

test_that("testing confint", {
  x <- stats::rnorm(1000, 10, 1)
  z <- stats::rnorm(1000, 5, 1)
  y<- stats::rnorm(1000, x, 1)
  fit1<-blm(y~x,1,1)
  blmRes <- confint(fit1)
  lmFit<-lm(y~x)
  lmRes <- stats::confint(lmFit)
  expect_equal(colnames(blmRes), colnames(lmRes))
  expect_equal(rownames(blmRes), rownames(lmRes))

})

test_that("testing predict", {
  x <- stats::rnorm(1000, 10, 1)
  y<- stats::rnorm(1000, x, 1)

  fit1<-blm(y~x,1,1)
  blmRes <- predict(fit1)

  expect_equal(y, blmRes, tolerance=0.1)

})
