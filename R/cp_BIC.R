#' Compute Bayesian information criterion
#'
#' Computes bayesian information criterion for comparing competing models.
#'
#' @param ll log-likelihood of the model of interest
#' @param k number of estimated parameters by the model
#' @param n sample size of data used to compute the log-likelihood
#' @return BIC stored as a numeric unit vector of length 1.
#' @author Edoardo Costantini, 2023
#' @examples
#' # Fit some model
#' lm_out <- lm(mpg ~ cyl + disp, data = mtcars)
#'
#' # Compute BIC with your function
#' BIC_M <- cp_BIC(
#'     ll = logLik(lm_out),
#'     n = nobs(lm_out),
#'     k = length(coef(lm_out)) + 1 # intercept + reg coefs + error variance
#' )
#' @export
cp_BIC <- function(ll, n, k) {
    # Compute measure
    BIC <- log(n) * k - 2 * ll

    # Return outcome
    as.numeric(BIC)
}