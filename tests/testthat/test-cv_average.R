# Project:   gspcr
# Objective: Testing the cv_average function
# Author:    Edoardo Costantini
# Created:   2023-03-16
# Modified:  2023-04-13
# Notes:

# Test: Dimensionality of the output -------------------------------------------

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
cv_average.out <- cv_average(
    cv_array = cv_array,
    fit_measure = "F"
)

# Check output is list
testthat::expect_true(is.list(cv_average.out))

# Check list is three elements long
testthat::expect_true(length(cv_average.out) == 3)

# Check score has expected dimensionality
testthat::expect_true(all(dim(cv_average.out$scor) == c(npcs, ntrhes)))

# Check score.upr has expected dimensionality
testthat::expect_true(all(dim(cv_average.out$score.upr) == c(npcs, ntrhes)))

# Check score.lwr has expected dimensionality
testthat::expect_true(all(dim(cv_average.out$score.lrw) == c(npcs, ntrhes)))

# Test: Desired behavior when input matrix has a whole -------------------------

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

# Define location of whole
loc_row <- 4
loc_col <- 2
loc_arr <- 5

# Put a whole in the matrix
cv_array[loc_row, loc_col, loc_arr] <- NA

# Compute the cv scores
cv_average.out <- cv_average(
    cv_array = cv_array,
    fit_measure = "F"
)

# Check resulting combined value is NA
testthat::expect_true(is.na(cv_average.out$scor[loc_row, loc_col]))