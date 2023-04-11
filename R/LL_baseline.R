#' Baseline category logistic regression log-likelihood
#'
#' Computes the baseline category logistic regression log-likelihood given a nominal categorical variable and the corresponding GLM linear predictor values.
#'
#' @param y A multi-categorical factor or its disjunctive table representation.
#' @param syst_comp A matrix storing all of the systematic components for a baseline category logistic regression.
#' @details
#' A disjunctive table is a matrix representation of a multi-categorical variable. The dimensionality of the matrix is i times j, with i = number of observations, and j = number of categories. \code{y_{ij}} is equal to 1 if observation i responded with category j, and it is equal to 0 otherwise.
#' @return Atomic vector of length 1 containing the log-likelihood value.
#' @author Edoardo Costantini, 2023
#'
#' @export
LL_baseline <- function(y, syst_comp) {
    # convert to numbers if needed
    if(is.factor(y)){
        y <- FactoMineR::tab.disjonctif(y)
    }

    # Define J
    J <- ncol(y)

    # Get rid of the baseline line for y
    y <- y[, -1, drop = FALSE]

    # Define a vector to store individual contributions to the likelihood
    contr_i <- rep(NA, nrow(y))
    
    # Compute individual contributions
    for (i in 1:nrow(y)) {
        contr_ij_pt1 <- rep(NA, J-1)
        contr_ij_pt2 <- rep(NA, J-1)
        for (j in 1:(J-1)) {
            contr_ij_pt1[j] <- y[i, j] * syst_comp[i, j]
            contr_ij_pt2[j] <- exp(syst_comp[i, j])
        }
        contr_i[i] <- sum(contr_ij_pt1, na.rm = TRUE) - log(1 + sum(contr_ij_pt2, na.rm = TRUE))
    }

    # Sum individual contribution to return the log-likelihood value
    sum(contr_i)

}
