#' Compute Akaike's information criterion
#'
#' Computes Akaike's information criterion for comparing competing models.
#'
#' @param ll log-likelihood of the model of interest
#' @param k number of estimated parameters by the model
#' @return AIC stored as a numeric unit vector of length 1.
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