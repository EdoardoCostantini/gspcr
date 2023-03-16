#' Cross-validation choice
#'
#' Extracting the CV choices of SPCR parameters.
#'
#' @param scor A description of the first argument
#' @param scor.lwr A description of the second argument
#' @param scor.upr A description
#' @param K A description
#' @param Test A description
#' @details
#' Given a matrix of npcs * thrsh, returns the best choice based on the type of test (best overall and 1se rule versions)
#' @return A list of two unit vectors:
#' - default = The default choice,
#' - oneSE = The choice based on the one standard error rule.
#' @author Edoardo Costantini, 2023
#' @references
#'
#' Such, S. (2006). Such and such. Journal such and such, 101(473), 119-137.
#'
#' @export
cv.choice <- function(scor, scor.lwr, scor.upr, K, test) {
    # Description: given an matrix of npcs * thrsh, returns the best choice based
    #              on the type of test (best overall and 1se rule versions)
    # Example internals:
    # scor = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
    # test = "F"
    # K = 10
    # scor.lwr = matrix(c(1, 2, 3, 4, 5, 6) - 1.5, nrow = 3, ncol = 2)
    # scor.upr = matrix(c(1, 2, 3, 4, 5, 6) + 1.5, nrow = 3, ncol = 2)

    # Decide if you need the max or the min
    if (test == "F" | test == "LRT" | test == "PR2") {
        maxmin <- "max"
    }
    if (test == "AIC" | test == "BIC" | test == "MSE") {
        maxmin <- "min"
    }

    # Extract the max or min value in the matrix
    choice <- eval(parse(text = paste0(maxmin, "(scor, na.rm = TRUE)")))

    # Return the coordinates of the choice
    cv.default <- which(scor == choice, arr.ind = TRUE)

    # Reverse engineer the standard error of the decision CV
    cv.default.se <- (scor - scor.lwr)[cv.default]

    # Logical matrix storing which values bigger than sol - 1SE
    if (test == "F" | test == "LRT" | test == "PR2") {
        scor.s1se <- scor >= choice - cv.default.se
    }
    # Logical matrix storing which values smaller than sol + 1SE
    if (test == "MSE" | test == "BIC" | test == "AIC") {
        scor.s1se <- scor <= choice + cv.default.se
    }

    # Logical matrix excluding default solution
    scor.ns <- scor != scor[cv.default]

    # Create a list of candidate models that are within 1 standard error of the best
    candidates <- which(scor.s1se & scor.ns, arr.ind = TRUE)

    # Attach value
    candidates <- cbind(candidates, values = scor[candidates[, 1:2]])

    # Are there such solutions?
    if (nrow(candidates) >= 1) {
        # Select the solutions with highest threshold (smallest number of predictors)
        candidates <- candidates[candidates[, "col"] == max(candidates[, "col"]), , drop = FALSE]

        # Select the solutions with lowest npcs (smallest number of components)
        candidates <- candidates[candidates[, "row"] == min(candidates[, "row"]), , drop = FALSE]

        # Select the solution with the smallest measure out of the candidate models
        cv.1se <- candidates[, -3]
    } else {
        cv.1se <- cv.default
    }

    return(
        list(
            default = cv.default,
            oneSE = cv.1se
        )
    )
}
