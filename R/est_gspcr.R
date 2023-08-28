#' Estimate Generalized Principal Component Regression
#'
#' Estimate SPCA on the data given chosen parameter values
#'
#' @param object \code{gspcrcv} object resulting from the call of \code{cv_gspcr()}. If this is specified, then every other argument can be left blank.
#' @param dv numeric vector or factor of dependent variable values
#' @param ivs \eqn{n \times p} data.frame of independent variables (factors allowed)
#' @param fam character vector of length 1 storing the description of the error distribution and link function to be used in the model
#' @param active_set names of the columns of ivs to be used as predictors
#' @param ndim numeric vector defining the number of principal components to be used (2 or more)
#' @details
#' After deciding on the number of components and the active set, this estimates the GSPCR model.
#' This function can be used by specifying the object argument or by filling in custom values for every argument. If both the object and any other argument are specified, then the argument values will be prioritized.
#' @return Description of function output
#' @author Edoardo Costantini, 2023
#' @references
#'
#' Bair, E., Hastie, T., Paul, D., & Tibshirani, R. (2006). Prediction by supervised principal components. Journal of the American Statistical Association, 101(473), 119-137.
#'
#' @export
est_gspcr <- function(object = NULL, dv, ivs, fam, active_set, ndim) {

    # Process object if not null
    if(!is.null(object)){
        # TODO: add check for presence of gspcr_call after including option to not save it!
        dv <- object$gspcr_call$dv
        ivs <- object$gspcr_call$ivs
        fam <- object$gspcr_call$fam
        active_set <- object$solution$standard$active_set
        ndim <- object$solution$standard$Q
    }

    # Reduced data matrix (active set)
    ivs_active <- ivs[, active_set, drop = FALSE]

    # Compute the first nidm PC of the reduced data matrix
    PCA_out <- pca_mix(
        X_tr = ivs_active,
        X_va = ivs_active,
        npcs = ndim
    )

    # Assemble data for easy use in future prediction functions
    data_glm <- data.frame(
        dv = dv,
        PCA_out$PC_tr
    )

    # Regress the outcome on the PCs (GLM framework)
    if (fam == "gaussian" | fam == "binomial" | fam == "poisson") {
        glm_fit <- stats::glm(
            formula = dv ~ .,
            family = fam,
            data = data_glm
        )
    }
    if (fam == "baseline") {
        glm_fit <- nnet::multinom(
            formula = dv ~ .,
            data = data_glm,
            trace = FALSE
        )
    }
    if (fam == "cumulative") {
        glm_fit <- MASS::polr(
            formula = dv ~ .,
            data = data_glm,
            method = "logistic"
        )
    }

    # Return the pca mix object and the trained model
    out <- list(
        glm_fit = glm_fit,
        fam = fam,
        pca = PCA_out,
        active_set = active_set,
        ndim = ndim
    )

    # Assign class to object
    class(out) <- c("gspcrout", "list")

    # Return it
    return(out)
}