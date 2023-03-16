#' Normal log-likelihood
#'
#' Computes the normal log-likelihood
#'
#' @param r Vector of residuals
#' @param s Residual standard deviation
#' @details
#' Given 
#' @return A matrix of the infile
#' @author Edoardo Costantini, 2022
#' @references
#'
#' Such, S. (2006). Such and such. Journal such and such, 101(473), 119-137.
#'
#' @export
loglike_norm <- function(r, s) {
    # Define n based on the residuals
    n <- length(r)

    # Compute the log-likelihood
    -n / 2 * log(2 * pi) - n / 2 * log(s^2) - 1 / (2 * s^2) * sum(r^2)
}
