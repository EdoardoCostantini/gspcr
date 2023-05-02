# Project:   gspcr
# Objective: Test gspcr computation
# Author:    Edoardo Costantini
# Created:   2023-05-01
# Modified:  2023-05-01
# Notes: 

# Test: PCA and gpca scores are the same on numeric data -----------------------

# Define numeric data
X <- iris[, 1:4]

# Number of PCs
npcs <- 3

# use GSPCA
T <- gpca(
    X_tr = X,
    npcs = npcs,
    scale = "standard"
)

# Scale data
X_scaled <- scale(X, center = TRUE, scale = TRUE)

# Perform PCA on the data
svd_X <- svd(X_scaled)

# Project training and validation data on the PCs
PC_X <- (X_scaled %*% svd_X$v)[, 1:npcs]

# Compute correlations between the npcs
cors <- sapply(1:npcs, function(j) {
    cor(PC_X[, j], T[, j])
})

# Check they are all equal to 1
testthat::expect_true(all(abs(cors) - 1 < tol))

# Test: gpca and PCAmixdata::PCAmix() are the same -----------------------------

# Define numeric data
X <- iris

# Number of PCs
npcs <- 4

# use GSPCA
T_mix <- gpca(
    X_tr = X,
    npcs = npcs,
    scale = "standard"
)

# Split data for PCA mix analysis
X_quanti <- PCAmixdata::splitmix(X)$X.quanti
X_quali <- PCAmixdata::splitmix(X)$X.quali

# Perform PCAmix with PCAmix package
pcamix <- PCAmixdata::PCAmix(
    X.quanti = X_quanti,
    X.quali = X_quali,
    rename.level = TRUE,
    ndim = npcs, 
    graph = FALSE
)

# Scores returned by PCAmixdata
pcamix$ind$coord

# Compute correlations between the npcs
cors_mix <- sapply(1:npcs, function(j) {
    cor(pcamix$ind$coord[, j], T_mix[, j])
})

# Check they are all equal to 1
testthat::expect_true(all(abs(cors_mix) - 1 > tol))
