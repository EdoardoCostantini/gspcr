#' Compute fit measure(s) on the validation data set
#'
#' Given a training and validation data set, it computes a target fit measure on the validation data set.
#'
#' @param y_train numeric vector or factor of dependent variable values from the training set
#' @param y_valid numeric vector or factor of dependent variable values from the validation set
#' @param X_train \eqn{n \times p} data.frame of independent variables (factors allowed) from the training set. Can also be set to NULL to obtain the log-likelihood of the new data under the null model.
#' @param X_valid \eqn{n \times p} data.frame of independent variables (factors allowed) from the validation set. If \code{X_train} is set to NULL to obtain the log-likelihood of the new data under the null model, \code{X_valid} is ignored.
#' @param fam character vector of length 1 storing the description of the error distribution and link function to be used in the model (see [gspcr::cv_gspcr()] for the list of possible options)
#' @param fit_measure character vector indicating which fit measure should be computed (see [gspcr::cv_gspcr()] for the list of possible options)
#' @details
#' The validation data set can be specified to be the same as the training data set if desired.
#' @return numeric vector of length 1 storing the requested fit measure
#' @author Edoardo Costantini, 2023
#' @examples 
#' # Example inputs
#' y_train = mtcars[1:20, 1]
#' y_valid = mtcars[-c(1:20), 1]
#' X_train = mtcars[1:20, -1]
#' X_valid = mtcars[-c(1:20), -1]
#' fam = "gaussian"
#' fit_measure = "BIC"
#' 
#' # Use the function
#' cp_validation_fit(y_train, y_valid, X_train, X_valid, fam, fit_measure)
#'
#' @export
cp_validation_fit <- function(y_train, y_valid, X_train, X_valid, fam, fit_measure) {

    # Estimate new data log-likelihoods under the model of interest
    mod_out <- LL_newdata(
        y_train = y_train,
        y_valid = y_valid,
        X_train = X_train,
        X_valid = X_valid,
        fam = fam
    )

    # Estimate new data log-likelihoods under the null model
    null_out <- LL_newdata(
        y_train = y_train,
        y_valid = y_valid,
        X_train = NULL,
        X_valid = NULL,
        fam = fam
    )

    # Extract desired fit measure
    if (fit_measure == "F") {
        # Compute F statistic with your function
        validation_fit <- cp_F(
            y = y_valid,
            y_hat_restricted = null_out$yhat_va,
            y_hat_full = mod_out$yhat_va,
            n = length(y_valid),
            p_restricted = 0,
            p_full = ncol(X_valid)
        )
    }
    if (fit_measure == "LRT") {
        validation_fit <- cp_LRT(
            ll_restricted = null_out$LL,
            ll_full = mod_out$LL
        )
    }
    if (fit_measure == "AIC") {
        validation_fit <- cp_AIC(
            ll = mod_out$LL,
            k = ncol(X_valid) + 1 + 1
        )
    }
    if (fit_measure == "BIC") {
        validation_fit <- cp_BIC(
            ll = mod_out$LL,
            n = length(y_valid),
            k = ncol(X_valid) + 2
        )
    }
    if (fit_measure == "PR2") {
        validation_fit <- cp_gR2(
            ll_n = null_out$LL,
            ll_f = mod_out$LL,
            n = length(y_valid)
        )
    }
    if (fit_measure == "MSE") {
        validation_fit <- MLmetrics::MSE(
            y_pred = mod_out$yhat_va,
            y_true = y_valid
        )
    }

    # Return validation fit
    return(validation_fit)
}