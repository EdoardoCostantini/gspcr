#' Generalized Principal Component Analysis
#'
#' A high level description of the function.
#'
#' @param X_tr data.frame of training data
#' @param X_new data.frame of new data if requested
#' @param npcs number of principal components to keep
#' @param scale either "standard" or "MLE"
#' @details
#' This function does such and such.
#' @return Description of function output
#' @author Edoardo Costantini, 2023
#' @examples
#' # Example inputs
#' data(wine, package = "FactoMineR")
#' X <- iris
#' train <- c(1:40, 50:90, 100:140)
#' valid <- -train
#' X_tr <- X[train, ]
#' X_new <- X[valid, ]
#' npcs <- 2
#' scale <- "standard"
#'
#' # Example use
#' gpca(
#'     X_tr = X[1:100, ],
#'     npcs = 2
#' )
#' @references
#'
#' Such, S. (2006). Such and such. Journal such and such, 101(473), 119-137.
#'
#' @export
gpca <- function(X_tr, npcs = 1, scale = "standard") {
    # Identify numeric variables
    num <- names(which(sapply(X_tr, is.numeric)))

    # Identify categorical variables
    fac <- names(which(sapply(X_tr, is.factor)))

    # Sample size
    n <- nrow(X_tr)

    # Total number of variables
    p <- ncol(X_tr)

    # Number of quantitive variables
    p1 <- length(num)

    # Number of qualitative variables
    p2 <- length(fac)

    # Group quantitive variables if any
    if (p1 > 0) {
        # Create quantitive objects
        X_tr_quanti <- X_tr[, num, drop = FALSE]

        # Scale quantitative variables
        if (scale == "standard") {
            Z1 <- scale(X_tr_quanti)
        }
        if (scale == "MLE") {
            Z1 <- apply(X_tr_quanti, 2, function(x) {
                sd_x <- sqrt(sum((x - mean(x))^2) / (length(x))) # not n-1!
                return((x - mean(x)) / sd_x)
            })
        }
    } else {
        Z1 <- NULL
    }

    # Group qualitative variables if any
    if (p2 > 0) {
        X_tr_quali <- X_tr[, fac, drop = FALSE]

        # Objects required for scaling
        p2 <- ncol(X_tr_quali)
        m <- sum(sapply(X_tr_quali, nlevels))
        G <- FactoMineR::tab.disjonctif(X_tr_quali)
        D <- diag(colSums(G))
        v_1 <- rep(1, n)
        I_n <- diag(v_1)
        J <- I_n - v_1 %*% t(v_1) / n

        # Scale qualitative variables
        Z2 <- J %*% G %*% solve(sqrt(D))
    } else {
        Z2 <- NULL
    }

    # Combine qunatitative and qualitative objects
    Z <- cbind(Z1, Z2) / sqrt(n)

    # General SVD
    svdZ <- svd(Z)
    L <- svdZ$d
    U <- svdZ$u
    V <- svdZ$v

    # Calculate Standardized Component Scores
    T <- (sqrt(n) * U)[, 1:npcs, drop = FALSE]

    # Loading vectors
    A <- V %*% diag(L)

    # Return scores
    return(T)
}