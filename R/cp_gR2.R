#' Compute generalized R-squared
#'
#' Computes the Cox and Snell generalized R-squared.
#'
#' @param ll_n numeric vector of length 1 (or an object of class 'logLik') storing the log-likelihood of the null (restricted) model
#' @param ll_f numeric vector of length 1 (or an object of class 'logLik') storing the log-likelihood of the full model
#' @param n numeric vector of length 1 storing the sample size of the data used to estimate the models
#' @details
#' The Cox and Snell generalized R-squared is equal to the R-squared when applied to multiple linear regression. The highest value for this measure is 1 - exp(ll_n)^(2/n), which is usually < 1.
#' The null (restricted) model must be nested within the full model.
#' @return numeric vector of length 1 storing the computed Cox and Snell generalized R-squared.
#' @author Edoardo Costantini, 2023
#' @references
#'
#' Allison, P. D. (2014, March). Measures of fit for logistic regression. In Proceedings of the SAS global forum 2014 conference (pp. 1-13). Cary, NC: SAS Institute Inc.
#'
#' @examples
#' # Fit a null model
#' lm_n <- lm(mpg ~ 1, data = mtcars)
#' 
#' # Fit a full model
#' lm_f <- lm(mpg ~ cyl + disp, data = mtcars)
#' 
#' # Compute generalized R2
#' gr2 <- cp_gR2(
#'     ll_n = as.numeric(logLik(lm_n)),
#'     ll_f = as.numeric(logLik(lm_f)),
#'     n = nobs(lm_f)
#' )
#' 
#' @export
cp_gR2 <- function(ll_n, ll_f, n) {
    
    # Compute the generalized R-squared
    gr2 <- 1 - exp(2 / n * (ll_n - ll_f))

    # Return it as an atomic numeric vector of length 1
    as.numeric(gr2)
}
