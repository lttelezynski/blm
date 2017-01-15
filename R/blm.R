
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
  if(!is.numeric(alpha)){
    stop("Alpha must be a number.")
  }
  if(alpha<=0){
    stop("Alpha must be greater than 0.")
  }
  if(!is.numeric(beta)){
    stop("Beta must be a number.")
  }
  if(beta<=0){
    stop("Alpha must be greater than 0.")
  }

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
                 functionCall=sys.call()
                 ),
                 class="blm")
}


distribution <- function(x) UseMethod("distribution")
distribution.default <- function(x) x
#distribution.blm <- function(x) x$posterior


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
make_prior <- function(model, alpha, ...) {
  n <- ncol(stats::model.frame(model, ...))
  return(list(mu=rep(0,n),Sigma=diag(1/alpha, nrow = n)))
}

posterior <- function(fit) fit$posterior


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
  mxy <- posterior(obj)$mu
  Sxy <- posterior(obj)$Sigma
  noResFor <- stats::delete.response(stats::terms(obj$formula))
  mf <- stats::model.frame(noResFor, ...)
  phiX <- stats::model.matrix(noResFor, ...)

  res <- vector(length = nrow(phiX))
  for(i in seq_along(res)){
    res[i] <- t(mxy)%*%phiX[i,]
  }
    return(res)
}


#' Get confidence intervals.
#'
#' Gets confidence intervals for coefficients from a fitted model.
#'
#' @param obj   A blm object.
#' @param parm   Parameters for which CI has to be calculated.
#' @param level   A parameter with CI level (0.95 for 95\% CI).
#'
#' @return A 2 rows matrix with confidence intervals.
#' @export
confint <- function(obj, parm, level=0.95){
  if(level>1){
    stop("Confidence interval cannot be greater than 1.")
  }
  if(level<0){
    stop("Confidence interval cannot be smaller than 0.")
  }

    int <- c((1-level)/2, 1-(1-level)/2)
    if(missing(parm)){
      n <- nrow(obj$coef)
      r<-matrix(nrow=n, ncol = 2)
      for(i in seq_along(obj$coef)){
        r[i,1]<-stats::qnorm(int[1], obj$coef[i,1], obj$var[i,i])
        r[i,2]<-stats::qnorm(int[2], obj$coef[i,1], obj$var[i,i])
      }
      rownames(r) <- rownames(obj$coef)
      colnames(r) <- c(paste((0.5-(level/2))*100, "%"), paste((0.5+(level/2))*100, "%"))
    }
    else{
      n <- length(parm)
      r<-matrix(nrow=n, ncol = 2)
      for(i in seq_along(parm)){
        r[i,1]<-stats::qnorm(int[1], obj$coef[parm[i],1], obj$var[parm[i],parm[i]])
        r[i,2]<-stats::qnorm(int[2], obj$coef[parm[i],1], obj$var[parm[i],parm[i]])
      }
      rownames(r) <- parm
      colnames(r) <- c(paste((0.5-(level/2))*100, "%"), paste((0.5+(level/2))*100, "%"))
    }
    return(r)
}

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


#' Get residuals.
#'
#' Gets residuals of the fit; the difference between predicted and observed values of the response.
#'
#' @param obj   A blm object.
#'
#' @return A vector with residuals.
#' @export
residuals <- function(obj){
  return(obj$data[,1]-fitted(obj))
}


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


#' Make a plot.
#'
#' Makes a plot with data points and a fitted line for models with one explanatory variable.
#'
#' @param obj   A blm object.
#'
#' @return A plot.
#' @export
plot <- function(obj){
  if(length(stats::model.frame(stats::delete.response(stats::terms(obj$formula))))!=1){
    stop("Plot function works only with models with one explanatory variable.")
  }
graphics::plot.default(obj$data[,2],obj$data[,1], xlab="x", ylab="y")
graphics::abline(obj$coef[1],obj$coef[2], col="red")
}


#' Print blm object.
#'
#' Prints function call and coefficients of blm object.
#'
#' @param x   A blm object.
#' @param ...   Additional parameters.
#'
#' @return Function call and coefficients.
#' @export
print.blm <- function(x, ...){
  cat("\nCall:\n")
  print(x$functionCall)
  cat("\nCoefficients:\n")
  print(coefficients(x))
}


#' Summary of blm object.
#'
#' Prints summary of blm object.
#'
#' @param object   A blm object.
#' @param ...   Additional parameters.
#'
#' @return Function call and coefficients.
#' @export
summary.blm <- function(object, ...){
  cat("\nCall:\n")
  print(object$functionCall)
  cat("\nResiduals:\n")
  q<- stats::quantile(residuals(object))
  names(q)<- c("Min", "1Q", "Median", "3Q", "Max")
  print(q)

  cat("\nCoefficients:\n")
  cof <-cbind(object$coef, confint(object))
  colnames(cof)<-c("Estimate", "Low 95% CI", "Upp 95% CI")
  print(cof)

  Rsq <- 1-(sum(residuals(object)^2)/(sum((object$data[,1]-mean(object$data[,1]))^2)))
  n=length(object$data[,1])
  p=length(object$data)-1
  df=n-1-p
  RSE <- sqrt(deviance(object)/df)
  cat("\nResidual standard error:", RSE, "on", df, "degrees of freedom")
  AdjRsq <- 1-(sum(residuals(object)^2)/(n-p-1)/(sum((object$data[,1]-mean(object$data[,1]))^2)/(n-1)))
  cat("\nR-squared:", Rsq, ",\tAdjusted R-squared: ", AdjRsq)

  MSM<-sum((fitted(object)-mean(object$data[,1]))^2)/p
  MSE<-sum(residuals(object)^2)/df
  Fstat <- MSM/MSE
  pval <- stats::pf(q=MSM/MSE, df1=p, df2=df,lower.tail = FALSE)
  cat("\nF-statistic:", Fstat, "on", p, "and", df, "DF, p-value: ", pval)
}