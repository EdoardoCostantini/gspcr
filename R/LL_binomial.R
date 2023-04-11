#' Binomial log-likelihood
#'
#' Computes the binomial log-likelihood given a response vector and corresponding GLM linear predictor values.
#'
#' @param y Vector (or factor) of values on a binary dependent variable.
#' @param syst_comp Vector of GLM linear predictor values.
#' @details
#' The log-likelihood equation is based on Agresti (2002, p. 192).
#' @return Atomic vector of length 1 containing the log-likelihood value.
#' @author Edoardo Costantini, 2022
#' @references
#'
#' Agresti, A. (2012). Categorical data analysis (Vol. 792). John Wiley & Sons.
#'
#' @export
LL_binomial <- function(y, syst_comp) {
    # convert to numbers if needed
    if(is.factor(y)){
        y <- stats::model.matrix(~y)[, -1]
    }

    # Compute the log-likelihood
    sum(y * syst_comp - log(1 + exp(syst_comp)))
}
