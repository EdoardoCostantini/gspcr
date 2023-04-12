#' Cumulative link model log-likelihood
#'
#' Computes the log-likelihood given an ordered categories response vector and corresponding GLM linear predictor values.
#'
#' @param y Vector (or factor) of values on a binary dependent variable.
#' @param syst_comp Matrix of GLM systematic component values.
#' @details
#' The log-likelihood equation is based on Agresti (2002, p. 192).
#' @return Atomic vector of length 1 containing the log-likelihood value.
#' @author Edoardo Costantini, 2022
#' @references
#'
#' Agresti, A. (2012). Categorical data analysis (Vol. 792). John Wiley & Sons.
#'
#' @export
LL_cumulative <- function(y, syst_comp) {
    # convert to numbers if needed
    if (is.factor(y)) {
        y <- FactoMineR::tab.disjonctif(y)
    }

    # Transform into cumulative probabilities
    cumsum <- cbind(0, exp(syst_comp) / (1 + exp(syst_comp)), 1)

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
    sum(sum(shelf))
}
