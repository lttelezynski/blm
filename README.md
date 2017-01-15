Bayesian linear regression
================
Lukasz Telezynski
2017-01-15

blm package
===========

Description
-----------

blm is a R package that can be used to fit bayesian linear models. It creates blm objects and is very similar to lm().

Usage example
-------------

First blm package needs to be loaded into R.

``` r
library(blm)
```

We need some data so we generate a dataset with 2 variables x and y.

``` r
x <- stats::rnorm(100, 10, 1)
y<- stats::rnorm(100, x, 1)
fit<-blm(y~x,1,1)
```

Finally, we can create blm object which fits linear model to our data with one explanatory variable x and response y. We need to specify formula and precision parameters alpha and beta.

``` r
formula <- y~x
alpha <- 1
beta <- 1
fit<-blm(formula,alpha,beta)
```

Now we can print some information about our fitted model,

``` r
summary(fit)
```

    ## 
    ## Call:
    ## blm(formula, alpha, beta)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -2.4962645 -0.8785048  0.1135928  0.6577995  2.5188096 
    ## 
    ## Coefficients:
    ##               Estimate Low 95% CI Upp 95% CI
    ## (Intercept) -0.1752364  -1.244565  0.8940927
    ## x            1.0171043   1.006451  1.0277579
    ## 
    ## Residual standard error: 1.073213 on 98 degrees of freedom
    ## R-squared: 0.4489373 ,   Adjusted R-squared:  0.4433142
    ## F-statistic: 74.93927 on 1 and 98 DF, p-value:  9.876267e-14

or plot some data

``` r
plot(fit)
```

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

Installation
------------

Since blm package is available only through github, devtools package can be used to install it.

1.  Install and load devtools package (ommit installation if already installed)

    ``` r
    install.packages("devtools")
    library(devtools)
    ```

2.  Install blm package using devtools

    ``` r
    install_github("lttelezynski/blm")
    ```

Functions
---------

1.  **blm()**
    Constructor creating object of class blm with a fitted model. Requires data and formula in format response~explanatory.

2.  **coefficients()**
    Given blm object as an argument, returns its coefficients.

3.  **confint()**
    Given blm object as an argument, returns confidence intervals of coefficients. Default level=0.95 (95% CI) is used. Any value between 0 and 1 can be used. Using parm parameter with name of specific coefficient or a vector with coefficients names, confidence intervals for only that coefficient or those coefficients can be aquired.

4.  **deviance()**
    Given blm object as an argument, returns sum of squared distances between observed and predicted response variables.

5.  **fitted()**
    Given blm object as an argument, returns its fitted values.

6.  **plot()**
    Given blm object as an argument, plots data and regression line for datasets with only one explanatory variable.

7.  **predict()**
    Given blm object as an argument, returns predictions of response variable. New data can be passed through ... argument.

8.  **print()**
    Given blm object as an argument, prints out function call and coefficients.

9.  **residuals()**
    Given blm object as an argument, returns residuals - differences between observed and fitted values of response variable.

10. **summary()**
    Given blm object as an argument, returns summary about fitted model.
    -   Function call
    -   Residuals (Min, 1Q, Median, 3Q, Max)
    -   Coefficients (Estimate with 95% confidence intervals)
    -   Residual standard error
    -   R² and Adjusted R²
    -   F-statistic and p-value
