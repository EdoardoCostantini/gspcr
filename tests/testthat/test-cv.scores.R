# Project:   gspcr
# Objective: Testing the cv_collect function
# Author:    Edoardo Costantini
# Created:   2023-03-16
# Modified:  2023-03-16
# Notes:

# Is the output of expected dimensionality? ------------------------------------

# Number of folds
K <- 5

# Number of thresholds
ntrhes <- 3

# Max number of PCs
npcs <- 10

# Define input array
cv_array <- array(
    data = abs(rnorm(n = npcs * ntrhes * K)),
    dim = c(npcs, ntrhes, K)
)

# Compute the cv scores
cv_collect.out <- cv_collect(
    cv_array = cv_array,
    test = "F"
)

# Check output is list
testthat::expect_true(is.list(cv_collect.out))

# Check list is three elements long
testthat::expect_true(length(cv_collect.out) == 3)

# Check score has expected dimensionality
testthat::expect_true(all(dim(cv_collect.out$scor) == c(npcs, ntrhes)))

# Check score.upr has expected dimensionality
testthat::expect_true(all(dim(cv_collect.out$score.upr) == c(npcs, ntrhes)))

# Check score.lwr has expected dimensionality
testthat::expect_true(all(dim(cv_collect.out$score.lrw) == c(npcs, ntrhes)))
