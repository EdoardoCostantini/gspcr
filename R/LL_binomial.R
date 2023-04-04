#' Normal log-likelihood
#'
#' Computes the binomial log-likelihood given a vector of probabilities
#'
#' @param r Vector of residuals
#' @param s Residual standard deviation
#' @details
#' Given
#' @return The log-likelihood value
#' @author Edoardo Costantini, 2022
#' @references
#'
#' Such, S. (2006). Such and such. Journal such and such, 101(473), 119-137.
#'
#' @export
LL_binomial <- function(y, lgt) {
    # convert to numbers if needed
    if(is.factor(y)){
        y <- model.matrix( ~ y)[, -1]
    }

    # Compute the log-likelihood
    sum(y * lgt - log(1 + exp(lgt)))
}
