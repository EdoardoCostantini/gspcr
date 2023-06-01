#' Predict GSPCR model dependent variable scores
#'
#' Predicts dependent variable values based on (new) predictor variables values.
#'
#' @param glm_fit An object of class \code{gspcr}.
#' @param pcamix A description of the second argument
#' @param x A description of the second argument
#' @param fam A description
#' @details
#' This function does such and such.
#' @return Description of function output
#' @author Edoardo Costantini, 2023
#' @examples
#' @export
predict.gspcrout <- function(glm_fit, pcamix, x = NULL, fam) {
    # Identify numeric variables
    num <- names(which(sapply(x, is.numeric)))

    # Identify categorical variables
    fac <- names(which(sapply(x, is.factor)))

    # Group quantitative variables if any
    if (length(num) > 0) {
        x_quanti <- x[, num, drop = FALSE]
    } else {
        x_quanti <- NULL
    }

    # Group qualitative variables if any
    if (length(fac) > 0) {
        x_quali <- x[, fac, drop = FALSE]
    } else {
        x_quali <- NULL
    }

    # Project new x on the PC space
    x_PC <- stats::predict(
        pcamix,
        X.quanti = x_quanti,
        X.quali = x_quali
    )

    # Assemble data
    data_glm <- data.frame(
        PCs = x_PC
    )

    # Predict new y based on the GLM estimated before
    if (fam == "gaussian" | fam == "binomial" | fam == "poisson") {
        # glm prediction
        y_hat <- predict(
            glm_fit,
            type = "response",
            newdata = data_glm
        )
    }
    if (fam == "baseline") {
        # nnet::multinom prediction
        y_hat <- predict(
            glm_fit,
            type = "prob",
            newdata = data_glm
        )
    }
    if (fam == "cumulative") {
        # MASS::polr prediction
        y_hat <- predict(
            glm_fit,
            type = "probs",
            newdata = data_glm
        )
    }

    # Return predicted values
    return(y_hat)
}