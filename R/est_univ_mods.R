#' Estimate simple GLM models
#'
#' Given a dependent variable, a set of possible predictors, and a GLM family, this function estimates a null GLM and all of the simple GLMs.
#'
#' @param dv numeric vector or factor of dependent variable values
#' @param ivs \eqn{n \times p} data.frame of independent variables (factors allowed)
#' @param fam character vector of length 1 storing the description of the error distribution and link function to be used in the model (see [gspcr::cv_gspcr()] for the list of possible options)
#' @return List containing:
#' - \code{ll0}: log-likelihoods for the null model
#' - \code{lls}: log-likelihoods for all the simple models
#' - \code{coefs}: if \code{dv} and \code{ivs} are continuous, standardized simple regression coefficients
#' @details 
#' We use the expression "simple GLM models" to describe GLM models with a single dependent variable and a single predictor.
#' @author Edoardo Costantini, 2023
#' @examples 
#' # Run the function on the example data set
#' dv_con_ivs_con <- est_univ_mods(
#'     dv = GSPCRexdata$y$cont,
#'     ivs = GSPCRexdata$X$cont,
#'     fam = "gaussian"
#' )
#'
#' @export
est_univ_mods <- function(dv, ivs, fam) {
    # Check which predictors are numeric
    numeric_preds <- apply(ivs, 2, is.numeric)

    # Standardize numeric predictors
    ivs[, numeric_preds] <- scale(ivs[, numeric_preds])

    # Standardize the dependent variable
    if (fam == "gaussian") {
        dv <- scale(dv)
    }

    # Train the null model and the p simple models
    if (fam == "gaussian" | fam == "binomial" | fam == "poisson") {
        # Fit null model
        glm0 <- stats::glm(dv ~ 1, family = fam)

        # Fit simple models
        glm.fits <- lapply(1:ncol(ivs), function(j) {
            stats::glm(dv ~ ivs[, j], family = fam)
        })
    }
    if (fam == "baseline") {
        glm0 <- nnet::multinom(
            formula = dv ~ 1,
            trace = FALSE
        )

        # Fit simple models
        glm.fits <- lapply(1:ncol(ivs), function(j) {
            tryCatch(
                expr = {
                    nnet::multinom(
                        formula = dv ~ ivs[, j],
                        trace = FALSE
                    )
                },
                error = function(e){
                    glm0
                }

            )
        })
    }
    if (fam == "cumulative") {
        glm0 <- MASS::polr(
            formula = dv ~ 1,
            method = "logistic"
        )

        # Fit simple models
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

    # Extract simple coefficients
    coefs <- sapply(glm.fits, function(m) as.numeric(stats::coefficients(m)[2]))

    # Return the values
    list(
        ll0 = ll0,
        lls = lls,
        coefs = coefs
    )
}