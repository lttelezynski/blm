
#' Bayesian linear model.
#'
#' Fits a model, given as a formula, optionally with data provided through the "..." parameter.
#'
#' @param model   A formula describing the model.
#' @param ...     Additional data, for example a data frame. Feel free to add other options.
#'
#' @return A fitted model.
#' @export
blm <- function(model, alpha, beta, ...) {
  # implement your function here...
  prior <- make_prior(model, alpha)
  posterior <- update(model, prior, beta, ...)

  structure(list(formula=model,
                 alpha=alpha,
                 beta=beta,
                 data=model.frame(model),
                 coef=posterior$mu,
                 var=posterior$Sigma,
                 prior=prior,
                 posterior=posterior,
                 class="blm"
                 ))
}


distribution <- function(x) UseMethod("distribution")
distribution.default <- function(x) x
distribution.blm <- function(x) x$posterior

update <- function(model, prior, beta, ...){

  prior <- distribution(prior)
  mf <- model.frame(model, ...)
  phiX <- model.matrix(model, ...)
  Sxy <- solve(prior$Sigma + beta*t(phiX)%*%phiX)
  mxy <- beta*Sxy%*%t(phiX)%*%mf[,1]
  return(list(mu=mxy, Sigma=Sxy))
}

library(MASS)
make_prior <- function(model, alpha, ...) {
  n <- ncol(model.frame(model, ...))
  return(list(mu=rep(0,n),Sigma=diag(1/alpha, nrow = n)))
}

posterior <- function(fit) fit$posterior

x <- rnorm(100, 10, 5)
z<- rnorm(100, 5, 5)
a <- 1
b <- 2
c<-3
y <- a+b*x

fit1 <- blm(y~x,1,1)
fit1$posterior


coefficients<-function(x){
  x$coef[,1]
}
cof<-coefficients(fit1)
cof

predict <- function(obj, ...){
  mxy <- obj$posterior$mu
  Sxy <- obj$posterior$Sigma
  noResFor <- delete.response(terms(obj$formula))
  mf <- model.frame(noResFor, ...)
  phiX <- model.matrix(noResFor, ...)

  res <- vector(length = nrow(phiX))
  for(i in seq_along(res)){
    res[i] <- t(mxy)%*%phiX[i,]
  }
    return(res)
}
predict(fit1)
y



confint <- function(obj, level){
    int <- c((1-level)/2, 1-(1-level)/2)
    n <- nrow(fit1$coef)
    r<-matrix(nrow=2, ncol = n)
    for(i in seq_along(obj$coef)){
      r[1,i]<-qnorm(int[1], obj$coef[i,1], obj$var[i,i])
      r[2,i]<-qnorm(int[2], obj$coef[i,1], obj$var[i,i])
    }
    return(r)
}

confint(fit1, level=0.5)


fitted <- function(obj){
  return(predict(obj))
}

fitted(fit1)


residuals <- function(obj){
  return(fitted(obj)-obj$data[,1])
}

residuals(fit1)
lmfit<-lm(y~x+z)
lmfit$residuals
lmfit$coefficients
coefficients(fit1)

lmfit$

deviance <- function(obj){
  return(sum(residuals(obj)^2))
}
deviance(fit1)


plot <- function(obj){
plot.default(obj$data[,2],obj$data[,1], xlab="x", ylab="y")
abline(obj$coef[1],obj$coef[2], col="red")
}

plot(fit1)
