#' PCA of a mixture of numerical and categorical data
#'
#' Wrapper for the `PCAmixdata::PCAmix()` function to be used in the main cross-validation procedure.
#'
#' @param X_tr data.frame of training data
#' @param X_va data.frame of validation data
#' @param npcs number of principal components to keep
#' @return a list of training and validation PC scores
#' @author Edoardo Costantini, 2023
#' @examples 
#' # Example inputs
#' data(wine, package = "FactoMineR")
#' X <- wine[, c(1, 2, 16, 22)]
#' X$Label <- factor(X$Label)
#' X$Soil <- factor(X$Soil)
#' X_tr <- X[1:15, ]
#' X_va <- X[16:21, ]
#' npcs <- 2
#' 
#' # Example use
#' pca_mix(
#'     X_tr = X[1:15, ],
#'     X_va = X[16:21, ],
#'     npcs = 2
#' )
#' @references
#'
#' Chavent M, Kuentz V, Labenne A, Liquet B, Saracco J (2017). PCAmixdata: Multivariate Analysis of Mixed Data. R package version 3.1, <https://CRAN.R-project.org/package=PCAmixdata>.
#'
#' @export
pca_mix <- function(X_tr, X_va, npcs = 1) {
    # Identify numeric variables
    num <- sapply(X_tr, is.numeric)

    # Identify categorical variables
    fac <- sapply(X_tr, is.factor)

    # Group quantitive variables if any
    if (any(num)) {
        X_tr_quanti <- X_tr[, num, drop = FALSE]
        X_va_quanti <- X_va[, num, drop = FALSE]
    } else {
        X_tr_quanti <- NULL
        X_va_quanti <- NULL
    }

    # Group qualitative variables if any
    if (any(fac)) {
        X_tr_quali <- X_tr[, fac, drop = FALSE]
        X_va_quali <- X_va[, fac, drop = FALSE]
    } else {
        X_tr_quali <- NULL
        X_va_quali <- NULL
    }

    # Perform PCAmix with PCAmix package
    pcamix <- PCAmixdata::PCAmix(
        X.quanti = X_tr_quanti,
        X.quali = X_tr_quali,
        rename.level = TRUE,
        ndim = max(2, npcs),
        graph = FALSE
    )
    
    # Project training data on the PCs
    PC_tr <- stats::predict(
        pcamix,
        X.quanti = X_tr_quanti,
        X.quali = X_tr_quali
    )[, 1:npcs, drop = FALSE]

    # Project validation data on the PCs
    PC_va <- stats::predict(
        pcamix,
        X.quanti = X_va_quanti,
        X.quali = X_va_quali
    )[, 1:npcs, drop = FALSE]

    # Return
    list(
        PC_tr = PC_tr,
        PC_va = PC_va,
        pcamix = pcamix
    )
}