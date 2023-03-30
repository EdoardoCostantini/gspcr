#' Collect results from CV
#'
#' A low-level function to average results from an array of CV results.
#'
#' @param cv_array A an array of q nt k dimensionality.
#' @param fit_measure The type of fit measure stored in the array
#' @details
#' The input of this function is an array of q nt k, where q is the number of principal components, nt is the number of thresholds and k is the number of folds.
#' @return A list of three q nt matrices.
#' - scor: contains the average CV scores across the K folds
#' - scor.upr: contains the average CV scores across the K folds + 1 standard deviation
#' - scor.lwr: contains the average CV scores across the K folds - 1 standard deviation
#' @author Edoardo Costantini, 2023
#' @references
#'
#' Such, S. (2006). Such and such. Journal such and such, 101(473), 119-137.
#'
#' @export
cv_collect <- function(cv_array, fit_measure) {
    # Description: given an array of npcs * thrsh * K dimensions, returns its average
    # Example internals:
    # - cv_array = array(abs(rnorm(10 * 3 * 2)), dim = c(10, 3, 2))
    # - fit_measure = "F"

    # How many folds?
    K <- utils::tail(dim(cv_array), 1)

    # Average selected score across folds
    if (fit_measure == "F") {
        # Average the log for a more symmetrical scale
        lscor <- apply(log(cv_array), c(1, 2), mean, na.rm = FALSE)

        # Compute standard error for each
        lscor.sd <- apply(log(cv_array), c(1, 2), stats::sd, na.rm = FALSE) / sqrt(K)

        # Revert to original scale and compute upper lower bounds
        scor <- exp(lscor)
        scor.upr <- exp(lscor + lscor.sd)
        scor.lwr <- exp(lscor - lscor.sd)
    } else {
        # Average normal results
        scor <- apply(cv_array, c(1, 2), mean, na.rm = FALSE)

        # Compute the standard errors
        scor.sd <- apply(cv_array, c(1, 2), stats::sd, na.rm = FALSE) / sqrt(K)

        # Compute the upper and lower bounds
        scor.upr <- scor + scor.sd
        scor.lwr <- scor - scor.sd
    }

    # Return
    list(
        scor = scor,
        scor.upr = scor.upr,
        scor.lwr = scor.lwr
    )
}