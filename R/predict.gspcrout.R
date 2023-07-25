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
        newdata <- object$ivs[, object$active_set, drop = FALSE]
    } else {
        newdata <- newdata[, object$active_set, drop = FALSE]
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
            # Error handler message
            print(
                paste0(
                    "The following error occurred when trying to project the new data on the PC axis: ",
                    "\"",
                    err,
                    "\"",
                    ". Do not worry, this was solved by augmenting the data with an extra row before projecting it, and then getting rid of the extra row before using the projected scores in the subsequent step. If you still get an error after this, then something else went wrong."
                ),
                quote = FALSE
            )

            # identify which variables have constant values
            contant_values <- sapply(x_quali, function(j) {
                length(unique(j)) == 1
            })

            # Create a fake data row
            x_quali_newraw <- x_quali[1, ]
            x_quanti_newraw <- x_quanti[1, ]

            # Add a different value for every variable impacted
            for (j in which(contant_values)) {
                # Extract the levels of the factor
                lvls <- levels(x_quali[, j])

                # identify the single used level
                used_lvl <- unique(x_quali[, j])

                # identify the first unused level (any other would do)
                unused_lvl <- lvls[!levels(x_quali[, j]) %in% used_lvl][1]

                # Replace it in the fake row
                x_quali_newraw[, j] <- unused_lvl
            }

            # Append the extra row to the new data
            x_quali <- rbind(x_quali, EXTRA = x_quali_newraw)
            x_quanti <- rbind(x_quanti, EXTRA = x_quanti_newraw)

            # Run PCAmix on the augmented data
            x_PC <- stats::predict(
                object$pca$pcamix,
                X.quanti = x_quanti,
                X.quali = x_quali
            )

            # Remove the extra row, if there was a problem with constant values
            x_PC <- x_PC[-nrow(x_PC), , drop = FALSE]

            # Return the scores
            return(x_PC)
        }
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