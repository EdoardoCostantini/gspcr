#' Cross-validation choice
#'
#' Extracting the CV choices of SPCR parameters.
#'
#' @param scor \eqn{npcs \times nthrs} matrix of K-fold CV scores
#' @param scor_lwr \eqn{npcs \times nthrs} matrix of score lower bounds
#' @param scor_upr \eqn{npcs \times nthrs} matrix of score upper bounds
#' @param K numeric vector of length 1 storing the number of folds for the K-fold cross-validation procedure
#' @param fit_measure character vector of length 1 indicating the type of fit measure to be used in the to cross-validation procedure
#' @details
#' Given a matrix of \eqn{npcs \times nthrs}, returns the best choice based on the type of fit measure (best overall and 1se rule versions.)
#' This function returns as solutions:
#' - \code{default}: the best choice based on the given fit measure (e.g. highest likelihood ratio test statistic, lowest BIC)
#' - \code{oneSE}: the solution that defined the most parsimonious model within 1 standard error from the best one. 
#' When both the number of components and the threshold parameter are cross-validated, the 1-standard error rule finds the candidate alternative solutions using the lowest number of PCs and having the best fit-measure. 
#' This decision is guided by the desire to counterbalance the tendency of GSPCR of selecting the highest number of components available when using cross-validation.
#' @return A list of two numeric vectors:
#' - \code{default}: numeric vector of length 2 that reports the coordinates in \code{scor} defining the default solution.
#' - \code{oneSE}: numeric vector of length 2 that reports the coordinates for \code{scor} defining the solution based on the one standard error rule
#' @author Edoardo Costantini, 2023
#' @examples 
#' # Score matrices
#' scor <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
#' scor_lwr <- matrix(c(1, 2, 3, 4, 5, 6) - 1.5, nrow = 3, ncol = 2)
#' scor_upr <- matrix(c(1, 2, 3, 4, 5, 6) + 1.5, nrow = 3, ncol = 2)
#' 
#' # Number of folds
#' K <- 10
#' 
#' # Type of fit_measure
#' fit_measure <- "F"
#' 
#' # Use the function
#' cv_choose(scor, scor_lwr, scor_upr, K, fit_measure)
#'
#' @export
cv_choose <- function(scor, scor_lwr, scor_upr, K, fit_measure) {

    # Decide if you need the max or the min
    if (fit_measure == "F" | fit_measure == "LRT" | fit_measure == "PR2") {
        maxmin <- "max"
    }
    if (fit_measure == "AIC" | fit_measure == "BIC" | fit_measure == "MSE") {
        maxmin <- "min"
    }

    # Extract the max or min value in the matrix
    choice <- eval(parse(text = paste0(maxmin, "(scor, na.rm = TRUE)")))

    # Return the coordinates of the choice
    cv.default <- which(scor == choice, arr.ind = TRUE)

    # If more than 1 coordinates have the same best value, choose the one using fewer variables because it's more efficient to compute
    if(sum(scor == choice, na.rm = TRUE) > 1){
        # First get the one with the highest col (smallest number of predictors involved)
        cv.default <- cv.default[cv.default[, "col"] == max(cv.default[, "col"]), , drop = FALSE]

        # Then get the one with the smallest row
        cv.default <- cv.default[cv.default[, "row"] == min(cv.default[, "row"]), , drop = FALSE]
    }

    # Reverse engineer the standard error of the decision CV
    cv.default.se <- (scor - scor_lwr)[cv.default]

    # Logical matrix storing which values bigger than sol - 1SE
    if (fit_measure == "F" | fit_measure == "LRT" | fit_measure == "PR2") {
        scor_s1se <- scor >= choice - cv.default.se
    }
    # Logical matrix storing which values smaller than sol + 1SE
    if (fit_measure == "MSE" | fit_measure == "BIC" | fit_measure == "AIC") {
        scor_s1se <- scor <= choice + cv.default.se
    }

    # Logical matrix excluding default solution
    scor_ns <- scor != scor[cv.default]

    # Create a list of candidate models that are within 1 standard error of the best
    candidates <- which(scor_s1se & scor_ns, arr.ind = TRUE)

    # Attach value
    candidates <- cbind(candidates, values = scor[candidates[, 1:2, drop = FALSE]])

    # Are there such solutions?
    if (nrow(candidates) >= 1) {
        # Select the solutions with lowest npcs (smallest number of components)
        candidates <- candidates[candidates[, "row"] == min(candidates[, "row"]), , drop = FALSE]

        # Select the candidate with the best fit
        if (fit_measure == "F" | fit_measure == "LRT" | fit_measure == "PR2") {
            candidates <- candidates[which.max(candidates[, "values"]), , drop = FALSE]
        }
        if (fit_measure == "MSE" | fit_measure == "BIC" | fit_measure == "AIC") {
            candidates <- candidates[which.min(candidates[, "values"]), , drop = FALSE]
        }

        # Store the solution 
        cv.1se <- candidates[, -3, drop = FALSE]
    } else {
        # Store default solution
        cv.1se <- cv.default
    }

    # Return the solutions
    return(
        list(
            default = cv.default[1, ],
            oneSE = cv.1se[1, ]
        )
    )
}
