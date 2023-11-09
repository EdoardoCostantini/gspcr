#' Compute Akaike's information criterion
#'
#' Computes Akaike's information criterion for comparing competing models.
#'
#' @param ll numeric vector of length 1 (or an object of class 'logLik') storing the log-likelihood of the model of interest
#' @param k numeric vector of length 1 storing the number of parameters estimated by the model
#' @return numeric vector of length 1 storing the computed AIC.
#' @author Edoardo Costantini, 2023
#' @examples
#' # Fit some model
#' lm_out <- lm(mpg ~ cyl + disp, data = mtcars)
#' 
#' # Compute AIC with your function
#' AIC_M <- cp_AIC(
#'     ll = logLik(lm_out),
#'     k = length(coef(lm_out)) + 1 # intercept + reg coefs + error variance
#' )
#' @export
cp_AIC <- function(ll, k) {
    # Compute measure
    AIC <- 2 * k - 2 * ll

    # Return outcome
    as.numeric(AIC)
}