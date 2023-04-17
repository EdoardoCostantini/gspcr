# Project:   gspcr
# Objective: Test cp_thrh_PR2
# Author:    Edoardo Costantini
# Created:   2023-04-17
# Modified:  2023-04-17
# Notes:

# Test: function works on continuous outcomes ----------------------------------

# Use the function
thrs_PR2 <- cp_thrs_PR2(
    dv = mtcars[, 1],
    ivs = mtcars[, -1],
    fam = "gaussian"
)

# Returns a numeric vector
testthat::expect_true(is.numeric(thrs_PR2))

# Values between 0 and 1
testthat::expect_true(all(thrs_PR2 >= 0 & thrs_PR2 <= 1))

# The vector has the names of the predictors
testthat::expect_true(all.equal(names(thrs_PR2), colnames(mtcars[, -1])))

# Test: function works on binary factor outcomes -------------------------------

# Use the function
thrs_bin_PR2 <- cp_thrs_PR2(
    dv = as.factor(mtcars[, 9]),
    ivs = mtcars[, -9],
    fam = "binomial"
)

# Returns a numeric vector
testthat::expect_true(is.numeric(thrs_bin_PR2))

# Negative values
testthat::expect_true(all(thrs_PR2 >= 0 & thrs_PR2 <= 1))

# The vector has the names of the predictors
testthat::expect_true(all.equal(names(thrs_bin_PR2), colnames(mtcars[, -9])))