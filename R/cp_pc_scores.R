#' Compute principal component scores
#'
#' Given a training and validation data set, it estimates the component loadings on the training data and returns the component scores on both the training and validation data.
#'
#' @param X_train Matrix of IV values in the training dataset.
#' @param X_valid Matrix of IV values in the validation dataset.
#' @param Q atomic numeric vector containing the number of principal components to be used
#' @details
#' This function does such and such.
#' @return Description of function output
#' @author Edoardo Costantini, 2023
#'
#' @export
cp_pc_scores <- function(X_train, X_valid, Q) {
    # Example inputs
    # X_train = as.matrix(mtcars[1:20, -1])
    # X_valid = as.matrix(mtcars[-c(1:20), -1])
    # Q = 3

    # If all variables are numeric, use regular PCA
    if (all(sapply(X_train, is.numeric))) {
        # Scale Xs
        X_train_thr <- scale(X_train, center = TRUE, scale = TRUE)
        X_valid_thr <- scale(X_valid,
            center = attributes(X_train_thr)$`scaled:center`,
            scale = attributes(X_train_thr)$`scaled:scale`
        )

        # Perform PCA on the training data
        svd_X_train <- svd(X_train_thr)

        # Project training and validation data on the PCs
        PC_tr <- X_train_thr %*% svd_X_train$v
        PC_va <- X_valid_thr %*% svd_X_train$v
    }

    # If some variables are not numeric (e.g., factors), use PCAmix
    if (!all(sapply(X_train, is.numeric))) {
        # Perform PCAmix
        pca_mix_out <- pca_mix(
            X_tr = X_train,
            X_va = X_valid,
            npcs = Q
        )

        # Extract objects of interest
        PC_tr <- pca_mix_out$PC_tr
        PC_va <- pca_mix_out$PC_va
    }

    # Select the desired number of PC scores
    list(
        PC_tr = PC_tr[, 1:Q, drop = FALSE],
        PC_va = PC_va[, 1:Q, drop = FALSE]
    )
}