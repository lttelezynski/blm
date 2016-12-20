
#' Bayesian linear model.
#'
#' Fits a model, given as a formula, optionally with data provided through the "..." parameter.
#'
#' @param model   A formula describing the model.
#' @param alpha   A parameter for prior distribution
#' @param beta    A parameter for posterior distribution
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
                 data=stats::model.frame(model),
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


#' Update prior to posterior distribution.
#'
#' Updates posterior distribution based on new prior/data
#'
#' @param model   A formula describing the model.
#' @param prior   Prior distribution
#' @param beta    A parameter for posterior distribution
#' @param ...     Additional data, for example a data frame.
#'
#' @return Posterior distribution as a list with mu and Sigma.
#' @export
update <- function(model, prior, beta, ...){

  prior <- distribution(prior)
  mf <- stats::model.frame(model, ...)
  phiX <- stats::model.matrix(model, ...)
  Sxy <- solve(prior$Sigma + beta*t(phiX)%*%phiX)
  mxy <- beta*Sxy%*%t(phiX)%*%mf[,1]
  return(list(mu=mxy, Sigma=Sxy))
}

#' Make prior distribution.
#'
#' Makes a prior distribution.
#'
#' @param model   A formula describing the model.
#' @param alpha   A parameter for prior distribution
#' @param ...     Additional data, for example a data frame.
#'
#' @return Prior distribution as a list with mu and Sigma..
#' @export
make_prior <- function(model, alpha, ...) {
  n <- ncol(stats::model.frame(model, ...))
  return(list(mu=rep(0,n),Sigma=diag(1/alpha, nrow = n)))
}

posterior <- function(fit) fit$posterior

x <- stats::rnorm(100, 10, 5)
z<- stats::rnorm(100, 5, 5)
a <- 1
b <- 2
c<-3
y <- a+b*x

fit1 <- blm(y~x,1,1)
fit1$posterior

#' Get coefficients of blm model.
#'
#' Gets coefficients of a blm model.
#'
#' @param obj   A blm object.
#'
#' @return A named vector with coefficients.
#' @export
coefficients<-function(obj){
  obj$coef[,1]
}
cof<-coefficients(fit1)
cof

#' Predict response.
#'
#' Predicts response based on fitted model, if no newdata is given, makes predictions on the data used to fit the model.
#'
#' @param obj   A blm object.
#' @param ...     Additional data, newdata used to predict response.
#'
#' @return A vector with predicted response.
#' @export
predict <- function(obj, ...){
  mxy <- obj$posterior$mu
  Sxy <- obj$posterior$Sigma
  noResFor <- stats::delete.response(stats::terms(obj$formula))
  mf <- stats::model.frame(noResFor, ...)
  phiX <- stats::model.matrix(noResFor, ...)

  res <- vector(length = nrow(phiX))
  for(i in seq_along(res)){
    res[i] <- t(mxy)%*%phiX[i,]
  }
    return(res)
}
pr<-predict(fit1)
y


#' Get confidence intervals.
#'
#' Gets confidence intervals for coefficients from a fitted model.
#'
#' @param obj   A blm object.
#' @param level   A parameter with CI level (0.95 for 95\% CI).
#'
#' @return A 2 rows matrix with confidence intervals.
#' @export
confint <- function(obj, level){
    int <- c((1-level)/2, 1-(1-level)/2)
    n <- nrow(fit1$coef)
    r<-matrix(nrow=2, ncol = n)
    for(i in seq_along(obj$coef)){
      r[1,i]<-stats::qnorm(int[1], obj$coef[i,1], obj$var[i,i])
      r[2,i]<-stats::qnorm(int[2], obj$coef[i,1], obj$var[i,i])
    }
    return(r)
}

confint(fit1, level=0.5)

#' Get fitted response.
#'
#' Gets fitted values from the model.
#'
#' @param obj   A blm object.
#'
#' @return A vector with fitted values.
#' @export
fitted <- function(obj){
  return(predict(obj))
}

fitted(fit1)

#' Get residuals.
#'
#' Gets residuals of the fit; the difference between predicted and observed values of the response.
#'
#' @param obj   A blm object.
#'
#' @return A vector with residuals.
#' @export
residuals <- function(obj){
  return(fitted(obj)-obj$data[,1])
}

residuals(fit1)
lmfit<-lm(y~x+z)
lmfit$residuals
lmfit$coefficients
coefficients(fit1)




#' Get deviance.
#'
#' Gets the sum of squared distances from the predicted to observed response.
#'
#' @param obj   A blm object.
#'
#' @return A deviance.
#' @export

deviance <- function(obj){
  return(sum(residuals(obj)^2))
}
deviance(fit1)

#' Make a plot.
#'
#' Makes a plot with data points and a fitted line for models with one explanatory variable.
#'
#' @param obj   A blm object.
#'
#' @return A plot.
#' @export
plot <- function(obj){
graphics::plot.default(obj$data[,2],obj$data[,1], xlab="x", ylab="y")
graphics::abline(obj$coef[1],obj$coef[2], col="red")
}

plot(fit1)

