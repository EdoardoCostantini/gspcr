#' Compute threshold values based on the pseudo R2
#'
#' Produces a vector of threshold values that define active predictors.
#'
#' @param dv Vector of dependent variable values
#' @param ivs Matrix of predictor values
#' @param fam GLM framework for the dv
#' 
#' @details
#' This function does such and such.
#' 
#' @return A vector of bivariate association measures between \code{dv} and \code{ivs}.
#' 
#' @author Edoardo Costantini, 2023
#' 
#' @examples
#' # Example inputs
#' dv <- mtcars[, 1]
#' ivs <- mtcars[, -1]
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

    # Give it good names
    names(CNR2) <- colnames(ivs)

    # Make them correlation coefficients
    ascores <- sqrt(CNR2)

    # Return
    ascores
}