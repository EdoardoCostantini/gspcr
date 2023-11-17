#' Predict GSPCR model dependent variable scores
#'
#' Predicts dependent variable values based on (new) predictor variables values.
#'
#' @param object An object of class \code{gspcr}.
#' @param newdata optionally, a data frame in which to look for variables with which to predict. If omitted, the fitted linear predictors are used.
#' @param ... further arguments passed to or from other methods.
#' @return Vector of prediction in "response" format for numerical data and probability of class membership for categorical data
#' @author Edoardo Costantini, 2023
#' @export
predict.gspcrout <- function(object, newdata = NULL, ...) {
    # Use input data if newdata is empty
    if (is.null(newdata)) {
        # Use the x_PCs that have already been computed
        x_PC <- object$pca$PC_tr
    } else {
        # Define the new active dataset
        newdata <- newdata[, object$active_set, drop = FALSE]

        # Identify numeric variables
        num <- sapply(newdata, is.numeric)

        # Identify categorical variables
        fac <- sapply(newdata, is.factor)

        # Group quantitative variables if any
        if (any(num)) {
            x_quanti <- newdata[, num, drop = FALSE]
        } else {
            x_quanti <- NULL
        }

        # Group qualitative variables if any
        if (any(fac)) {
            x_quali <- newdata[, fac, drop = FALSE]
        } else {
            x_quali <- NULL
        }

        # Project new data on the PC space
        x_PC <- tryCatch(
            {
                # Normal behavior
                x_PC <- stats::predict(
                    object$pca$pcamix,
                    X.quanti = x_quanti,
                    X.quali = x_quali
                )
                # TODO: The CRAN version of PCAmixdata currently has a bug where if a
                # new dataset is provided with a categorical column having all the
                # same values then new projections are not returned.
                # This is undesirable.
                # Currently, I handle the error here because the package authors are
                # aware of the bug. They have actually fixed it in the github version.
                # However, my package relies on the cran version.
                # Therefore, as of right now I need the workaround implemented in
                # this tryCatch().
                # Whenever the PCAmixdata package maintainers update the CRAN version
                # I can get rid of this and just keep the simple normal predict behavior.
            },
            error = function(err) {

                # Predict one at the time to avoid any problem
                x_PC <- lapply(seq_along(1:nrow(newdata)), function(i) {
                    stats::predict(
                        object$pca$pcamix,
                        X.quanti = x_quanti[i, , drop = FALSE],
                        X.quali = x_quali[i, , drop = FALSE]
                    )
                })

                # Put predictions together in a single matrix-like object
                x_PC <- do.call(rbind, x_PC)

                # Return the scores
                return(x_PC)
            }
        )
    }

    # Assemble data
    data_glm <- data.frame(
        x_PC
    )

    # Predict new y based on the GLM estimated before
    if (object$fam == "gaussian" | object$fam == "binomial" | object$fam == "poisson") {
        # glm prediction
        y_hat <- stats::predict(
            object$glm_fit,
            type = "response",
            newdata = data_glm
        )
    }
    if (object$fam == "baseline") {
        # nnet::multinom prediction
        y_hat <- stats::predict(
            object$glm_fit,
            type = "prob",
            newdata = data_glm
        )
    }
    if (object$fam == "cumulative") {
        # MASS::polr prediction
        y_hat <- stats::predict(
            object$glm_fit,
            type = "probs",
            newdata = data_glm
        )
    }

    # Return predicted values
    return(y_hat)
}