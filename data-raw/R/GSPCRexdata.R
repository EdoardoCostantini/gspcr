# Project:   gspcr
# Objective: Script to generate some example data for the package
# Author:    Edoardo Costantini
# Created:   2023-03-16
# Modified:  2023-03-31
# Notes:     After updating the script and the RDS file, to update the data in 
#            the package you need to run again usethis::use_data(GSPCRexdata) call

# Functions --------------------------------------------------------------------

generateXTP <- function(I, J, VAFr = c(.5, .4, .2), VAFsum = 100, CPVE = 0.9) {
    # Internals -------------------------------------------------------------

    # I    = 100 # sample size
    # J    = 9 # number of variables
    # VAFr = c(.5, .3, .2) # relative variance of each components
    # VAFsum = 100 # total variance of the components
    # CPVE = 0.9 # proportion of explained variance by the R components

    # Body ------------------------------------------------------------------
    # Number of components
    R <- length(VAFr)

    # Random sample U
    U <- matrix(
        data = rnorm(I * R),
        nrow = I,
        ncol = R
    )
    U <- scale(U, center = TRUE, scale = FALSE)
    U <- orthmat(U, verbose = FALSE)
    U <- normmat(U)

    # Random sample P
    V <- matrix(
        data = runif(J * R),
        nrow = J,
        ncol = R
    )
    V <- orthmat(V, verbose = FALSE)
    P <- normmat(V)

    # Define D
    D <- diag(c(VAFsum * VAFr))

    # Create X
    Xtrue <- U %*% D %*% t(P)

    # sample from normal distribution (Ex = Error of X)
    Ex <- MASS::mvrnorm(n = I, mu = rep(0, J), Sigma = diag(J))

    # centering and scaling the Ex matrix
    Ex <- scale(Ex, center = TRUE, scale = FALSE)

    # sum of squares
    ssqXtrue <- sum(Xtrue^2)
    ssqEx <- sum(Ex^2)

    # Compute noise scaling constant
    Escale <- sqrt(ssqXtrue * (1 - CPVE) / (CPVE * ssqEx))

    # Add scaled noise
    X <- Xtrue + Escale * Ex

    # Scale data for estimation
    X <- scale(X)

    # Define outputs
    return(list(
        X = data.frame(X),
        T = U %*% D,
        P = P,
        U = U,
        D = D
    ))
}

generateDV <- function(X = matrix(), R2 = 0.90, beta = 1) {
    # Internals -------------------------------------------------------------

    # X     = matrix(rnorm(1e3 * 4), nrow = 1e3, ncol = 4)
    # R2    = .9
    # beta  = 1

    # Body ------------------------------------------------------------------
    # Generate a dependent variable on true line
    y_true <- as.vector(X %*% rep(beta, ncol(X)))

    # Generate random error
    error <- rnorm(nrow(X))

    # Make the error orthogonal to the Xs
    e_ortho <- orthmat(cbind(X, error))[, "error"]

    # sum of squares
    ssqy <- sum(X^2)
    ssqe <- sum(e_ortho^2)

    # Rescale noise to desired level
    e_scale <- sqrt(ssqy * (1 - R2) / (R2 * ssqe))

    # Generate samples for y
    y <- y_true + e_scale * e_ortho

    # What to return
    return(y)
}

normmat <- function(X) {
    # Internals -------------------------------------------------------------

    # X    = matrix(rnorm(1e3 * 4), nrow = 1e3, ncol = 4)

    # Body ------------------------------------------------------------------
    X <- apply(X, 2, function(j) j / sqrt(sum(j^2)))
    return(X)
}

orthmat <- function(X, verbose = FALSE) {
    # Internals -------------------------------------------------------------

    # X    = matrix(rnorm(1e3 * 4), nrow = 1e3, ncol = 4)

    # Body ------------------------------------------------------------------
    for (i in 2:ncol(X)) {
        for (j in 1:(i - 1)) {
            if (verbose == TRUE) {
                print(paste0("Adjusting piar ", i, "-", j))
            }
            A <- X[, j]
            b <- X[, i]

            # Find projection of b on A
            B <- as.vector(b - (t(A) %*% b / t(A) %*% A) %*% A)

            # Replace in original X the orthogonalized columns
            X[, j] <- A
            X[, i] <- B
        }
    }
    return(X)
}

# Generate data ----------------------------------------------------------------

# Define the desired number of components for X
Q <- 5

# Define the desired number of components predicting y
Qy <- 2

# Desired sample size
N <- 1e3

# Desired number of variables
P <- 50

# Sed seed
set.seed(2026)

# Generate data
XTP <- generateXTP(
    I <- N, # sample size
    J <- P, # number of variables
    VAFr <- diff(seq(0, 1, len = Q + 1)), # relative variance of each components
    VAFsum <- 100, # total variance of the components
    CPVE <- 0.3 # proportion of explained variance by the R components
)

# Compute PCA with prcomp
PCX <- prcomp(XTP$X)

# Extract eigenvalues
eigenvalues <- PCX$sdev^2

# Cumulative proportion of explained variance
cumsum(prop.table(eigenvalues))[1:Q]

# Non-graphical solutions
nFactors::nScree(x = eigenvalues)

# Screeplot
nFactors::plotuScree(x = eigenvalues)

# > Continuous -----------------------------------------------------------------

# Generate DV based on the component scores
y <- generateDV(
    X = XTP$T[, 1:Qy, drop = FALSE],
    R2 = 0.80,
    beta = 1
)

# Create other versions of this variable
u <- pnorm(y)

# > Binary ---------------------------------------------------------------------
y_bin <- factor(
    x = qbinom(u, size = 1, prob = .7),
    levels = 0:1,
    labels = LETTERS[1:2]
)
table(y_bin)

# > Ordinal variable -----------------------------------------------------------
K <- 5

# Create lags
lags <- rep(abs(min(y) - max(y)) / K, (K - 1))

# Define the break points y
breaks <- c(cumsum(c(minimum = min(y), fixed = lags)), maximum = max(y))

# Cut y with the given brakes
y_dis <- as.numeric(cut(x = y, breaks = breaks, include.lowest = TRUE))

# Make an ordered factor
y_ord <- factor(
    x = y_dis,
    ordered = TRUE
)
table(y_ord)

# > Multi-categorical ----------------------------------------------------------
K <- 3
y_cat <- factor(
    x = qbinom(u, size = K - 1, prob = c(.1, .3, .6)),
    levels = 0:(K - 1),
    labels = letters[1:K]
)
table(y_cat)

# Poisson
y_pois <- qpois(u, lambda = 2)
factor(
    x = y_pois,
    levels = sort(unique(y_pois)),
    labels = length(unique(y_pois))
)

# Store results ----------------------------------------------------------------

# Collect data in a single data.frame
GSPCRexdata <- list(
    X = XTP$X,
    y = data.frame(
        cont = y,
        bin = y_bin,
        ord = y_ord,
        cat = y_cat,
        pois = y_pois
    )
)

# Make use of this data in the package
usethis::use_data(GSPCRexdata, overwrite = TRUE)

# Save example dataset (note really needed but useful to have somewhere)
saveRDS(GSPCRexdata, "./data-raw/data/GSPCRexdata.rds")