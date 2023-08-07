#' Compute threshold values based on the pseudo R2
#'
#' Produces a vector of threshold values that define active predictors.
#'
#' @param dv numeric vector or factor of dependent variable values
#' @param ivs \eqn{n \times p} data.frame of independent variables (factors allowed)
#' @param fam character vector of length 1 storing the description of the error distribution and link function to be used in the model (see [gspcr::cv_gspcr()] for the list of possible options)
#' @return A vector of bivariate association measures between \code{dv} and \code{ivs}.
#' @author Edoardo Costantini, 2023
#' @examples
#' # Example inputs
#' dv <- mtcars[, 1]
#' ivs <- mtcars[, -1]
#' 
#' # Use the function
#' cp_thrs_PR2(dv, ivs, fam = "gaussian")
#' 
#' @export
cp_thrs_PR2 <- function(dv, ivs, fam) {
    # Estimate the univariate models
    univ_mods <- est_univ_mods(
        dv = dv,
        ivs = ivs,
        fam = fam
    )

    # Compute pseudo R-squared
    CNR2 <- 1 - exp(-2 / nrow(ivs) * (univ_mods$lls - univ_mods$ll0))

    # Safeguard against CNR2 so small that computer could make negative
    CNR2 <- abs(CNR2)

    # Give it good names
    names(CNR2) <- colnames(ivs)

    # Make them correlation coefficients
    ascores <- sqrt(CNR2)

    # Return
    ascores
}