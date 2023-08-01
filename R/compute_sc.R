#' Compute the GLM systematic component.
#'
#' Compute the systematic component of a GLM of interest.
#'
#' @param mod a fit object returned by one of `stats::lm`, `stats::glm`, `nnet::multinom`, or `MASS::polr`.
#' @param predictors matrix or data.frame of predictor values to compute the systematic component based on.
#' @details
#' This function takes different model objects and knows how to treat the coefficient vector (or matrix) to obtain the systematic component.
#'
#' @return a matrix of \eqn{n \times k}, where \eqn{k} is equal to 1 for all but multi-categorical models. This matrix contains the systematic component values for the provided predictors.
#' @author Edoardo Costantini, 2023
#'
#' @export
#'
compute_sc <- function(mod, predictors) {
    # Number of expected regression coefficients + intercept
    p <- ncol(predictors) + 1

    # Is this a null model?
    is_mod_null <- all(predictors == 1)

    # Extract the coefficient estimates
    B <- stats::coef(mod)

    # If this is a polr object
    if (any("polr" == class(mod))) {
        # Compute the systematic component (aj - bx)
        if (is_mod_null == TRUE) {
            sc <- matrix(
                data = rep(mod$zeta, nrow(predictors)),
                nrow = nrow(predictors),
                byrow = TRUE
            )
        } else {
            # Prepare X
            x <- stats::model.matrix(
                object = ~.,
                data = predictors
            )[, -1, drop = FALSE]

            # Compute bx
            bx <- x %*% matrix(mod$coefficients)

            # Compute aj - bx
            sc <- sapply(mod$zeta, function(a) {
                a - bx
            })
        }
    } else {
        # If this is a nnet output, then take the transpose
        if (any(c("multinom", "nnet") %in% class(mod))) {
            B <- t(B)
        }

        # Compute the systematic component
        if (is_mod_null == TRUE) {
            sc <- as.matrix(predictors) %*% B
        } else {
            sc <- stats::model.matrix(~., predictors) %*% B
        }
    }

    # Return systematic component (numeric vector)
    sc
}
