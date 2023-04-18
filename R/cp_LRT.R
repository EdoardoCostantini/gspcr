#' Compute likelihood ratio test
#'
#' Computes the likelihood ratio expressed as a difference between the log-likelihoods of observed data under two nested competing models.
#'
#' @param ll_restricted log-likelihood of the observed data under the restricted model
#' @param ll_full log-likelihood of the observed data under the full model
#' @details
#' Note that:
#' - The full model is always the model with more estimated parameters, the model with more predictor variables.
#' - The restricted model is the model with fewer estimated parameters.
#' - The restricted model must be nested within the full model.
#' @return Description of function output
#' @author Edoardo Costantini, 2023
#' @export
cp_LRT <- function(ll_restricted, ll_full){

    # Compute
    LRT <- -2 * (ll_restricted - ll_full)

    # Return
    as.numeric(LRT)

}