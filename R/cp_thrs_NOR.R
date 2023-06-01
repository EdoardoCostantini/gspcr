#' Compute normalized association measure
#'
#' A function to compute the normalized bivariate association measures between a \code{dv} and a collection of \code{ivs}.
#'
#' @param dv numeric vector of dependent variable values
#' @param ivs \eqn{n \times p} matrix of numeric independent variables
#' @param scale_dv logical value defining whether \code{dv} should be scaled
#' @param scale_ivs logical value defining whether \code{ivs} should be scaled
#' @param s0_perc numeric vector of length 1 storing the factor for the denominator of association statistic (i.e., the percentile of standard deviation values added to the denominator, a value between 0 and 1.) The default is 0.5 (the median)
#' @details
#' This function is based on the function \code{cor.func} in the package \code{superpc}.
#' @return numeric vector of bivariate association measures between \code{dv} and \code{ivs}. numeric vector of log-likelihood value from all of the univariate GLM models regressing \code{dv} on each column of \code{ivs}.
#' @author Edoardo Costantini, 2023
#' @references
#' 
#' Bair E, Hastie T, Paul D, Tibshirani R (2006). “Prediction by supervised principal components.” J. Am. Stat. Assoc., 101(473), 119-137.
#'
#' @examples
#' # Example inputs
#' dv <- mtcars[, 1]
#' ivs <- mtcars[, -1]
#' s0_perc <- 0
#' 
#' # Use the function
#' cp_thrs_NOR(dv, ivs, s0_perc)
#' 
#' @export
cp_thrs_NOR <- function(dv, ivs, s0_perc = NULL, scale_dv = TRUE, scale_ivs = TRUE) {

    # Pre-process data if requested
    if(scale_dv == TRUE){
        dv  <- scale(dv)
    }
    if (scale_ivs == TRUE) {
        ivs <- scale(ivs)
    }

    # Set objects to the required dimension
    x <- t(as.matrix(ivs))
    y <- dv

    # Sample size
    n <- length(y)

    # Compute vector of feature means
    xbar <- as.vector(x %*% rep(1 / n, n))

    # Covariance between Xs 
    sxx <- (x - xbar)^2 %*% rep(1, n)

    # Covariance between x and y (no denominator)
    sxy <- (x - xbar) %*% (y - mean(y))

    # Ratio of the two
    numer <- sxy / sxx

    # Total sum of squares
    syy <- sum((y - mean(y))^2)

    # Compute sd?
    stdev <- sqrt((syy / sxx - numer^2) / (n - 2))

    # add "fudge"(?) to the denominator
    if (is.null(s0_perc)) {
        fudge <- stats::median(stdev)
    }
    if (!is.null(s0_perc)) {
        if (s0_perc > 0) {
            fudge <- stats::quantile(stdev, s0_perc)
        }
        if (s0_perc == 0) {
            fudge <- 0
        }
    }

    # Ratio between numerator and stdev
    tt <- numer / (stdev + fudge)

    # Store the normalized correlation scores
    ascores <- as.numeric(abs(tt))

    # Give it good names
    names(ascores) <- colnames(ivs)

    # Return
    ascores
}