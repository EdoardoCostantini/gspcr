#' Gaussian log-likelihood
#'
#' Computes the gaussian (normal) log-likelihood of a vector of observed values given a trained linear regression model.
#'
#' @param y Vector of observed values on a continuous dependent variable.
#' @param syst_comp Vector of predicted values by the model.
#' @param mod \code{glm} or \code{lm} object containing and estimated linear regression model.
#' @return Atomic vector of length 1 containing the log-likelihood value.
#' @author Edoardo Costantini, 2022
#'
#' @export
LL_gaussian <- function(y, syst_comp, mod) {

    # Store the ML estimate of the sigma from the model
    s <- sqrt(sum(stats::resid(mod)^2) / (stats::nobs(mod)))

    # Obtain residuals for the target ys (can be different from model)
    r <- (y - syst_comp)

    # Define n based on the residuals
    n <- length(r)

    # Compute the log-likelihood
    -n / 2 * log(2 * pi) - n / 2 * log(s^2) - 1 / (2 * s^2) * sum(r^2)
}
