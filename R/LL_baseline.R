#' Baseline category logistic regression log-likelihood
#'
#' Computes the baseline category logistic regression log-likelihood given a nominal categorical variable and the corresponding GLM linear predictor values.
#'
#' @param y factor or disjunctive table representation recording a nominal variable with 3 or more categories.
#' @param x data.frame (or matrix) containing predictor values.
#' @param mod \code{multinom} object containing the estimated baseline-category logit model.
#' @details
#' If \code{x} and \code{y} are equal to the data on which \code{mod} has been trained, this function returns the same result as the default \code{logLink} function. If \code{x} and \code{y} are new, the function returns the log-likelihood of the new data under the trained model.
#'
#' A disjunctive table is a matrix representation of a multi-categorical variable. The dimensionality of the matrix is i times j, with i = number of observations, and j = number of categories. \code{y_{ij}} is equal to 1 if observation i responded with category j, and it is equal to 0 otherwise.
#' The log-likelihood equation is based on Agresti (2002, p. 192).
#' @return A list containing:
#' - \code{ll} atomic vector of length 1 containing the log-likelihood value.
#' - \code{sc} numeric matrix containing the systematic component for the input \code{x} and \code{mod}.
#' @author Edoardo Costantini, 2023
#' @references
#'
#' Agresti, A. (2012). Categorical data analysis (Vol. 792). John Wiley & Sons.
#'
#' @export
LL_baseline <- function(y, x, mod) {
    # Compute the GLM systematic component
    sc <- compute_sc(
        mod = mod,
        predictors = x
    )

    # Convert y to a dummy code representation
    if (is.factor(y)) {
        y_dc <- stats::model.matrix(~y)[, -1, drop = FALSE]
    } else {
        # Check if y is provided as a matrix of ncol(sc) columns
        if (is.matrix(y)) {
            if (ncol(y) == ncol(sc)) {
                y_dc <- y
            } else {
                if(ncol(y) == (ncol(sc) + 1)){
                    y_dc <- y[, -1, drop = FALSE]
                }
            }
        } else {
            # Transform y into a factor and make the dummy code representation
            y_dc <- stats::model.matrix(~ factor(y))[, -1, drop = FALSE]
        }
    }

    # Define sample size
    n <- nrow(y_dc)

    # Define J = ncat - 1 or ncol of sc
    J <- ncol(sc)

    # Define a vector to store individual contributions to the likelihood
    contr_i <- rep(NA, n)

    # Compute individual contributions
    for (i in 1:n) {
        # Define storing objects for (parts of the) individual likelihood contributions
        contr_ij_pt1 <- rep(NA, J)
        contr_ij_pt2 <- rep(NA, J)

        # Populate the individual likelihood contribution parts
        for (j in seq_along(1:J)) {
            contr_ij_pt1[j] <- y_dc[i, j] * sc[i, j]
            contr_ij_pt2[j] <- exp(sc[i, j])
        }

        # Create individual likelihood contributions
        contr_i[i] <- sum(contr_ij_pt1, na.rm = TRUE) - log(1 + sum(contr_ij_pt2, na.rm = TRUE))
    }

    # Sum individual contributions to return the log-likelihood value
    ll <- sum(contr_i)

    # Return
    list(
        ll = ll,
        sc = sc
    )
}
