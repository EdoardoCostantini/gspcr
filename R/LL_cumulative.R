#' Proportional odds model log-likelihood
#'
#' Computes the log-likelihood given an ordered category response vector and corresponding GLM linear predictor values.
#'
#' @param y ordered factor or disjunctive table representation recording an ordinal variable with 3 or more categories.
#' @param x data.frame (or matrix) containing predictor values.
#' @param mod \code{polr} object containing the estimated proportional odds model.
#' @details
#' If \code{x} and \code{y} are equal to the data on which \code{mod} has been trained, this function returns the same result as the default \code{logLink} function. If \code{x} and \code{y} are new, the function returns the log-likelihood of the new data under the trained model.
#' The log-likelihood equation is based on Agresti (2002, p. 192).
#' @return A list containing:
#' - \code{ll} an atomic vector of length 1 containing the log-likelihood value.
#' - \code{sc}, a numeric matrix containing the systematic component for the input \code{x} and \code{mod}.
#' @author Edoardo Costantini, 2022
#' @references
#'
#' Agresti, A. (2012). Categorical data analysis (Vol. 792). John Wiley & Sons.
#'
#' @export
LL_cumulative <- function(y, x, mod) {
    # Compute the GLM systematic component
    sc <- compute_sc(
        mod = mod,
        predictors = x
    )

    # convert to numbers if needed
    if (is.factor(y)) {
        y <- FactoMineR::tab.disjonctif(y)
    }

    # Transform into cumulative probabilities
    cumsum <- cbind(0, exp(sc) / (1 + exp(sc)), 1)

    # Define a storing matrix
    shelf <- matrix(nrow = nrow(cumsum), ncol = ncol(cumsum) - 1)

    # Compute individual contributions
    for (i in 1:nrow(cumsum)) {
        # i <- 1
        for (j in 2:ncol(cumsum)) {
            # j <- 2
            shelf[i, j - 1] <- y[i, j - 1] * log(cumsum[i, j] - cumsum[i, j - 1])
        }
    }

    # And then I can compute the log-likelihood
    ll <- sum(sum(shelf))

    # Return
    list(
        ll = ll,
        sc = sc
    )
}
