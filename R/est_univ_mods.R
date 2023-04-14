#' Estimate univariate standard GLMs.
#'
#' Given a dependent variable, a set of possible predictors and a GLM family, this function estimates a null GLM and all of the univariate GLMs.
#'
#' @param dv Vector of dependent variable values
#' @param ivs Matrix of predictor values
#' @param fam GLM framework for the dv
#' @details
#' This function does such and such.
#' @return A list of:
#' - \code{ll0}: log-likelihoods for the null model
#' - \code{lls}: log-likelihoods for all the univariate models
#' - \code{coefs}: if \code{dv} and \code{ivs} are continuous, standardized univariate regression coefficients
#' @author Edoardo Costantini, 2023
#'
#' @export
est_univ_mods <- function(dv, ivs, fam) {

    # Check which predictors are numeric
    numeric_preds <- apply(ivs, 2, is.numeric)

    # Standardize numeric predictors
    ivs[, numeric_preds] <- scale(ivs[, numeric_preds])

    # Standardize the dependent variable
    if(fam == "gaussian"){
        dv <- scale(dv)
    }

    # Train the null model and the p univariate models
    if (fam == "gaussian" | fam == "binomial" | fam == "poisson") {
        # Fit null model
        glm0 <- stats::glm(dv ~ 1, family = fam)

        # Fit univariate models
        glm.fits <- lapply(1:ncol(ivs), function(j) {
            stats::glm(dv ~ ivs[, j], family = fam)
        })
    }
    if (fam == "baseline") {
        glm0 <- nnet::multinom(
            formula = dv ~ 1
        )

        # Fit univariate models
        glm.fits <- lapply(1:ncol(ivs), function(j) {
            nnet::multinom(dv ~ ivs[, j])
        })
    }
    if (fam == "cumulative") {
        glm0 <- MASS::polr(
            formula = dv ~ 1,
            method = "logistic"
        )

        # Fit univariate models
        glm.fits <- lapply(1:ncol(ivs), function(j) {
            MASS::polr(
                formula = dv ~ ivs[, j],
                method = "logistic"
            )
        })
    }

    # Extract Log-likelihood values
    ll0 <- as.numeric(stats::logLik(glm0))
    lls <- sapply(glm.fits, function(m) as.numeric(stats::logLik(m)))

    # Give it good names
    names(lls) <- colnames(ivs)

    # Extract univariate coefficients
    coefs <- sapply(glm.fits, function(m) as.numeric(stats::coefficients(m)[2]))

    # Return the values
    list(
        ll0 = ll0,
        lls = lls,
        coefs = coefs
    )

}