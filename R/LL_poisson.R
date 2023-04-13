#' Poisson regression log-likelihood
#'
#' Computes the Poisson regression log-likelihood of a vector of observed values given the GLM systematic component.
#'
#' @param y Vector of values on a count dependent variable.
#' @param syst_comp Vector of GLM linear predictor values.
#' @return Atomic vector of length 1 containing the log-likelihood value.
#' @author Edoardo Costantini, 2023
#' @references
#'
#' Agresti, A. (2012). Categorical data analysis (Vol. 792). John Wiley & Sons.
#'
#' @export
LL_poisson <- function(y, syst_comp){

    # Compute the log-likelihood
    -sum(exp(syst_comp)) + sum(y * syst_comp) - sum(log(factorial(y)))

}