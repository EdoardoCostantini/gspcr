#' Compute likelihood ratio test
#'
#' Computes the likelihood ratio expressed as a difference between the log-likelihoods of observed data under two nested competing models.
#'
#' @param ll_restricted numeric vector of length 1 (or an object of class 'logLik') storing the log-likelihood of the observed data under the restricted model
#' @param ll_full numeric vector of length 1 (or an object of class 'logLik') storing the log-likelihood of the observed data under the full model
#' @details
#' Note that:
#' - The full model is always the model with more estimated parameters, the model with more predictor variables.
#' - The restricted model is the model with fewer estimated parameters.
#' - The restricted model must be nested within the full model.
#' @return numeric vector of length 1 storing the likelihood ratio test statistic
#' @author Edoardo Costantini, 2023
#' @examples
#' # Fit a nested model
#' nested <- glm(mpg ~ cyl + disp, data = mtcars)
#' 
#' # Fit a complex model
#' complex <- glm(mpg ~ cyl + disp + hp + am, data = mtcars)
#' 
#' # Compute log-likelihood statistic with your function
#' LRT_M <- cp_LRT(
#'     ll_restricted = logLik(nested),
#'     ll_full = logLik(complex)
#' )
#' @export
cp_LRT <- function(ll_restricted, ll_full){

    # Compute
    LRT <- -2 * (ll_restricted - ll_full)

    # Return
    as.numeric(LRT)

}