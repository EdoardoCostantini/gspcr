# Project:   gspcr
# Objective: Test cp_thrh_NOR
# Author:    Edoardo Costantini
# Created:   2023-04-17
# Modified:  2023-04-17
# Notes:

# Test: function works on continuous outcomes ----------------------------------

# Use the function
thrs_NOR <- cp_thrs_NOR(
    dv = mtcars[, 1],
    ivs = mtcars[, -1],
    s0_perc = NULL
)

# Returns a numeric vector
testthat::expect_true(is.numeric(thrs_NOR))

# Values are positive
testthat::expect_true(all(thrs_NOR >= 0))

# The vector has the names of the predictors
testthat::expect_true(all.equal(names(thrs_NOR), colnames(mtcars[, -1])))
