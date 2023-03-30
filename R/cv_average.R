#' Average fit measures computed in the K-fold cross-validation procedure
#'
#' A low-level function to average results from an array of K-fold CV fit measures.
#'
#' @param cv_array A an array of q nt k dimensionality.
#' @param fit_measure The type of fit measure stored in the array
#' @details
#' The input of this function is an array of q nt k, where q is the number of principal components, nt is the number of thresholds and k is the number of folds.
#' @return A list of three q nt matrices.
#' - scor: contains the average CV scores across the K folds
#' - scor_upr: contains the average CV scores across the K folds + 1 standard deviation
#' - scor_lwr: contains the average CV scores across the K folds - 1 standard deviation
#' @author Edoardo Costantini, 2023
#' @references
#'
#' Such, S. (2006). Such and such. Journal such and such, 101(473), 119-137.
#'
#' @export
cv_average <- function(cv_array, fit_measure) {
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
        lscor_sd <- apply(log(cv_array), c(1, 2), stats::sd, na.rm = FALSE) / sqrt(K)

        # Revert to original scale and compute upper lower bounds
        scor <- exp(lscor)
        scor_upr <- exp(lscor + lscor_sd)
        scor_lwr <- exp(lscor - lscor_sd)
    } else {
        # Average normal results
        scor <- apply(cv_array, c(1, 2), mean, na.rm = FALSE)

        # Compute the standard errors
        scor_sd <- apply(cv_array, c(1, 2), stats::sd, na.rm = FALSE) / sqrt(K)

        # Compute the upper and lower bounds
        scor_upr <- scor + scor_sd
        scor_lwr <- scor - scor_sd
    }

    # Return
    list(
        scor = scor,
        scor_upr = scor_upr,
        scor_lwr = scor_lwr
    )
}