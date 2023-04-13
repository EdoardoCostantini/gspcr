#' Poisson regression log-likelihood
#'
#' Computes the Poisson regression log-likelihood of a vector of observed values given the GLM systematic component.
#'
#' @param y numeric vector recording a count dependent variable.
#' @param x data.frame (or matrix) containing predictor values.
#' @param mod \code{glm} object containing the estimated poisson regression model.
#' @details
#' If \code{x} and \code{y} are equal to the data on which \code{mod} has been trained, this function returns the same result as the default \code{logLink} function. If \code{x} and \code{y} are new, the function returns the log-likelihood of the new data under the trained model.
#' @return Atomic vector of length 1 containing the log-likelihood value.
#' @author Edoardo Costantini, 2023
#' @references
#'
#' Agresti, A. (2012). Categorical data analysis (Vol. 792). John Wiley & Sons.
#'
#' @export
LL_poisson <- function(y, x, mod) {
    # Compute the GLM systematic component
    sc <- compute_sc(
        mod = mod,
        predictors = x
    )

    # Compute the log-likelihood
    -sum(exp(sc)) + sum(y * sc) - sum(log(factorial(y)))
}