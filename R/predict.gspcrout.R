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
    if(is.null(newdata)){
        newdata <- object$ivs[, object$active_set]
    } else {
        newdata <- newdata[, object$active_set]
    }

    # Identify numeric variables
    num <- names(which(sapply(newdata, is.numeric)))

    # Identify categorical variables
    fac <- names(which(sapply(newdata, is.factor)))

    # Group quantitative variables if any
    if (length(num) > 0) {
        x_quanti <- newdata[, num, drop = FALSE]
    } else {
        x_quanti <- NULL
    }

    # Group qualitative variables if any
    if (length(fac) > 0) {
        x_quali <- newdata[, fac, drop = FALSE]
    } else {
        x_quali <- NULL
    }

    # Project new newdata on the PC space
    x_PC <- stats::predict(
        object$pca$pcamix,
        X.quanti = x_quanti,
        X.quali = x_quali
    )

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