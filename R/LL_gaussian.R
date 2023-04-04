#' Normal log-likelihood
#'
#' Computes the normal log-likelihood
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
LL_gaussian <- function(y, y_hat, mod) {

    # Store the ML estimate of the sigma from the model
    s <- sqrt(sum(stats::resid(mod)^2) / (nobs(mod)))

    # Obtain residuals for the target ys (can be different from model)
    r <- (y - y_hat)

    # Define n based on the residuals
    n <- length(r)

    # Compute the log-likelihood
    -n / 2 * log(2 * pi) - n / 2 * log(s^2) - 1 / (2 * s^2) * sum(r^2)
}
