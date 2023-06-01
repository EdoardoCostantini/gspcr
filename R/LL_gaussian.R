#' Gaussian log-likelihood
#'
#' Computes the gaussian (normal) log-likelihood of a vector of observed values given a trained linear regression model.
#'
#' @param y numeric vector recording a continuous dependent variable.
#' @param x data.frame (or matrix) containing predictor values.
#' @param mod \code{glm} or \code{lm} object containing the estimated linear regression model.
#' @details 
#' If \code{x} and \code{y} are equal to the data on which \code{mod} has been trained, this function returns the same result as the default \code{logLink} function. If \code{x} and \code{y} are new, the function returns the log-likelihood of the new data under the trained model.
#' @return A list containing:
#' - \code{ll} an atomic vector of length 1 containing the log-likelihood value.
#' - \code{sc} an atomic vector containing the systematic component for the input \code{x} and \code{mod}.
#' @author Edoardo Costantini, 2022
#'
#' @export
LL_gaussian <- function(y, x, mod) {
    # Compute the GLM systematic component
    sc <- compute_sc(
        mod = mod,
        predictors = x
    )

    # Store the ML estimate of the sigma from the model
    s <- sqrt(sum(stats::resid(mod)^2) / (stats::nobs(mod)))

    # Obtain residuals for the target ys (can be different from model)
    r <- (y - sc)

    # Define n based on the residuals
    n <- length(r)

    # Compute the log-likelihood
    ll <- -n / 2 * log(2 * pi) - n / 2 * log(s^2) - 1 / (2 * s^2) * sum(r^2)

    # Return
    list(
        ll = ll,
        sc = sc
    )
}
