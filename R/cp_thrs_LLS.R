#' Compute threshold values based on Log-likelihood values
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
cp_thrs_LLS <- function(dv, ivs, fam) {
    # Estimate the univariate models
    univ_mods <- est_univ_mods(
        dv = dv,
        ivs = ivs,
        fam = fam
    )

    # Extract log-likelihood scores
    ascores <- univ_mods$lls

    # Return
    ascores
}