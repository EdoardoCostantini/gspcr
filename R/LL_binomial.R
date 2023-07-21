#' Binomial log-likelihood
#'
#' Computes the binomial log-likelihood given a response vector and corresponding GLM linear predictor values.
#'
#' @param y numeric vector (or factor) recording a binary dependent variable.
#' @param x data.frame (or matrix) containing predictor values.
#' @param mod \code{glm} object containing and estimated logistic regression model.
#' @details
#' If \code{x} and \code{y} are equal to the data on which \code{mod} has been trained, this function returns the same result as the default \code{logLink} function. If \code{x} and \code{y} are new, the function returns the log-likelihood of the new data under the trained model.
#' The log-likelihood equation is based on Agresti (2002, p. 192).
#' @return A list containing: 
#' - \code{ll} an atomic vector of length 1 containing the log-likelihood value.
#' - \code{sc} an atomic vector containing the systematic component for the input \code{x} and \code{mod}.
#' @author Edoardo Costantini, 2022
#' @references
#'
#' Agresti, A. (2012). Categorical data analysis (Vol. 792). John Wiley & Sons.
#'
#' @export
LL_binomial <- function(y, x, mod) {
    # Compute the GLM systematic component
    sc <- compute_sc(
        mod = mod,
        predictors = x
    )

    # convert y to numbers if needed
    if (is.factor(y)) {
        y <- stats::model.matrix(~y)[, -1]
    }

    # Compute the log-likelihood
    ll <- sum(y * sc - log(1 + exp(sc)))

    # Return
    list(
        ll = ll,
        sc = sc
    )
}
