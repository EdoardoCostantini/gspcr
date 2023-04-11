#' Compute the GLM systematic component.
#'
#' A low-level function to compute the systematic component of a GLM of interest.
#'
#' @param mod A fit object returned by either glm or nnet::multinom.
#' @param predictors Dataset of predictor values to compute the systematic component based on.
#' @details
#' This function takes different model objects and knows how to treat the coefficient vector (or matrix) to obtain the systematic component.
#' 
#' `data` should be a data.frame with the first column being the dependent variable on which the model has been trained. This first variable should be called `y`.
#' 
#' @return A matrix of \eqn{n \times k}, where \eqn{k} is equal to 1 for all but multi-categorical models. This matrix contains the systematic component values for the provided predictors.
#' @author Edoardo Costantini, 2023
#' 
#' @export
#' 
compute_sc <- function(mod, predictors) {
    # Number of expected regression coefficients + intercept
    p <- ncol(predictors) + 1

    # Extract the coefficient estimates
    B <- stats::coef(mod)

    # Are the coefficients a vector?
    is_B_vector <- is.vector(B)

    # If this is a nnet output, then take the transpose
    if (any(c("multinom", "nnet") %in% class(mod))) {
        B <- t(B)
    }

    # Is this a null model?
    is_mod_null <- all(predictors == 1)

    # Compute the systematic component
    if(is_mod_null == TRUE){
        sc <- as.matrix(predictors) %*% B
    } else {
        sc <- stats::model.matrix(~., predictors) %*% B
    }

    # Return systematic component (numeric vector)
    sc
}
