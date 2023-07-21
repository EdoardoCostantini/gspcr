#' Average fit measures computed in the K-fold cross-validation procedure
#'
#' Function to average results from an array of K-fold CV fit measures.
#'
#' @param cv_array \eqn{Q \times nthrs \times K} array containing fit measures computed for different combinations of the number of components, threshold values, and number of CV-folds.
#' @param fit_measure character vector of length 1 indicating the type of fit measure to be used in the to cross-validation procedure
#' @details
#' The input of this function is an array of \eqn{Q \times nthrs \times K}, where \code{Q} is the number of principal components, \code{nthrs} is the number of thresholds, and \code{K} is the number of folds.
#' @return list of three \eqn{Q \times nthrs} matrices:
#' - \code{scor}: contains the average CV scores across the K folds
#' - \code{scor_upr}: contains the average CV scores across the K folds + 1 standard deviation
#' - \code{scor_lwr}: contains the average CV scores across the K folds - 1 standard deviation
#' @author Edoardo Costantini, 2023
#' @examples 
#' # Example inputs
#' cv_array = array(abs(rnorm(10 * 3 * 2)), dim = c(10, 3, 2))
#' fit_measure = "F"
#' 
#' # Use the function
#' cv_average(cv_array, fit_measure)
#' 
#' @export
cv_average <- function(cv_array, fit_measure) {
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