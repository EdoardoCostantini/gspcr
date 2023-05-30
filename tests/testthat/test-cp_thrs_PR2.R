# Project:   gspcr
# Objective: Test cp_thrh_PR2
# Author:    Edoardo Costantini
# Created:   2023-04-17
# Modified:  2023-05-30
# Notes:

# Define tolerance
tol <- 1e-5

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

# Values between 0 and 1
testthat::expect_true(all(thrs_PR2 >= 0 & thrs_PR2 <= 1))

# The vector has the names of the predictors
testthat::expect_true(all.equal(names(thrs_bin_PR2), colnames(mtcars[, -9])))

# Test: results independent of input scaling -----------------------------------

# Unscaled inputs
thrs_PR2 <- cp_thrs_PR2(
    dv = GSPCRexdata$y$cont * 10 + 10,
    ivs = GSPCRexdata$X$cont,
    fam = "gaussian"
)

# Scaled X
thrs_PR2_scaled_X <- cp_thrs_PR2(
    dv = GSPCRexdata$y$cont * 10 + 10,
    ivs = scale(GSPCRexdata$X$cont),
    fam = "gaussian"
)

# Scaled y
thrs_PR2_sacled_y <- cp_thrs_PR2(
    dv = scale(GSPCRexdata$y$cont),
    ivs = GSPCRexdata$X$cont,
    fam = "gaussian"
)

# Scaled y and X
thrs_PR2_scaled_X_sacled_y <- cp_thrs_PR2(
    dv = scale(GSPCRexdata$y$cont),
    ivs = scale(GSPCRexdata$X$cont),
    fam = "gaussian"
)

# Perform tests
testthat::expect_true(all((thrs_PR2 - thrs_PR2_scaled_X) < tol))
testthat::expect_true(all((thrs_PR2 - thrs_PR2_sacled_y) < tol))
testthat::expect_true(all((thrs_PR2 - thrs_PR2_scaled_X_sacled_y) < tol))