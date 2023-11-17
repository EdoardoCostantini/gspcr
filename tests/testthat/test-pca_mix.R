# Project:   gspcr
# Objective: Test pca_mix.R function
# Author:    Edoardo Costantini
# Created:   2023-05-02
# Modified:  2023-11-17
# Notes: 

# Define tolerance
tol <- 1e-5

# Test: PCAmix = PCA on numeric data -------------------------------------------

# Load data
data(wine, package = "FactoMineR")

# Define only numeric data
X <- wine[, -c(1:2)]

# Define training and validation datasets
npcs <- 5

# Apply PCA_mix function
pca_mix_out <- pca_mix(
    X_tr = X,
    X_va = X,
    npcs = npcs
)

# Perform regular PCA
X_centered <- scale(X, center = TRUE, scale = FALSE)
X_scaled <- apply(X_centered, 2, function(j){
    j / (sd(j) * (nrow(X_centered)-1) / nrow(X_centered) )
})

# Perform PCA on the training data
svd_Xtr <- svd(X_scaled)

# Project training and validation data on the PCs
PC_tr <- (X_scaled %*% svd_Xtr$v)[, 1:npcs]

# Compute correlations between the npcs
cors <- sapply(1:npcs, function(j){
    cor(PC_tr[, j], pca_mix_out$PC_tr[, j])
})

# Check they are all equal to 1
testthat::expect_true(all(abs(cors) - 1 < tol))

# Test: validation data projections as expected on numeric data ----------------

# Define a training validation index
train <- 1:15
valid <- -train

# Use pca_mix on training and validation
pca_mix_out_tv <- pca_mix(
    X_tr = X[train, ],
    X_va = X[valid, ],
    npcs = npcs
)

# Scale training data
X_tr_scaled <- scale(X[train, ], center = TRUE, scale = TRUE)

# Scale validation data based on training data
X_va_scaled <- scale(
    x = X[valid, ],
    center = attributes(X_tr_scaled)$`scaled:center`,
    scale = attributes(X_tr_scaled)$`scaled:scale`
)

# Perform PCA on the training data
svd_Xtr_tv <- svd(X_tr_scaled)

# Project training on the PCs
PC_tr_tv <- (X_tr_scaled %*% svd_Xtr_tv$v)[, 1:npcs]

# Project validation data on the PCs
PC_va_tv <- (X_va_scaled %*% svd_Xtr_tv$v)[, 1:npcs]

# Compute correlations between the two training PCs
cors_tr <- sapply(1:npcs, function(j) {
    cor(PC_tr_tv[, j], pca_mix_out_tv$PC_tr[, j])
})

# Compute correlations between the two validation PCs
cors_va <- sapply(1:npcs, function(j) {
    cor(PC_va_tv[, j], pca_mix_out_tv$PC_va[, j])
})

# Check they are all equal to 1
testthat::expect_true(all(abs(cors_tr) - 1 < tol))
testthat::expect_true(all(abs(cors_va) - 1 < tol))

# Test: npcs = 1 works with mixed data -----------------------------------------

# Define a training validation index
train <- sample(1:nrow(iris))[1:100]
valid <- -train

# Check runs without errors
testthat::expect_no_error(
    pca_mix(
        X_tr = iris[train, ],
        X_va = iris[valid, ],
        npcs = 1
    )
)

# Test: treatment of ordered factor is as nominal variables --------------------

# Use pca_mix on ordinal data
pca_mix_ordinal <- tryCatch(
    pca_mix_out_tv <- pca_mix(
        X_tr = GSPCRexdata$X$ord,
        X_va = GSPCRexdata$X$ord,
        npcs = 2
    ),
    error = function(e) e
)

# Use pca_mix on nominal data
pca_mix_nominal <- tryCatch(
    pca_mix_out_tv <- pca_mix(
        X_tr = GSPCRexdata$X$cat,
        X_va = GSPCRexdata$X$cat,
        npcs = 2
    ),
    error = function(e) e
)

# Check is the result an error?
testthat::expect_equal(pca_mix_ordinal, pca_mix_nominal)