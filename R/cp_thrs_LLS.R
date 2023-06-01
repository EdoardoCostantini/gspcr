#' Compute threshold values based on Log-likelihood values
#'
#' Produces a vector of threshold values that define active predictors.
#'
#' @param dv numeric vector or factor of dependent variable values
#' @param ivs \eqn{n \times p} data.frame of independent variables (factors allowed)
#' @param fam character vector of length 1 storing the description of the error distribution and link function to be used in the model (see [gspcr::cv_gspcr()] for the list of possible options)
#' @return numeric vector of log-likelihood value from all of the univariate GLM models regressing \code{dv} on each column of \code{ivs}.
#' @author Edoardo Costantini, 2023
#' @examples
#' # Example inputs
#' dv <- mtcars[, 1]
#' ivs <- mtcars[, -1]
#' fam <- "gaussian"
#' 
#' # Use function
#' cp_thrs_LLS(dv, ivs, fam)
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