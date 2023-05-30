# Project:   gspcr
# Objective: Test cp_thrh_NOR
# Author:    Edoardo Costantini
# Created:   2023-04-17
# Modified:  2023-05-30
# Notes:

# Define tolerance
tol <- 1e-5

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

# Test: results relationship to input scaling ----------------------------------

# Unscaled inputs
thrs_NOR <- cp_thrs_NOR(
    dv = GSPCRexdata$y$cont * 10 + 10,
    ivs = GSPCRexdata$X$cont,
    scale_dv = FALSE,
    scale_ivs = FALSE,
    s0_perc = NULL
)

# Scaled X
thrs_NOR_scaled_X <- cp_thrs_NOR(
    dv = GSPCRexdata$y$cont * 10 + 10,
    ivs = GSPCRexdata$X$cont,
    scale_dv = FALSE,
    scale_ivs = TRUE,
    s0_perc = NULL
)

# Scaled y
thrs_NOR_sacled_y <- cp_thrs_NOR(
    dv = GSPCRexdata$y$cont,
    ivs = GSPCRexdata$X$cont,
    scale_dv = TRUE,
    scale_ivs = FALSE,
    s0_perc = NULL
)

# Scaled y and X
thrs_NOR_scaled_X_sacled_y <- cp_thrs_NOR(
    dv = GSPCRexdata$y$cont,
    ivs = GSPCRexdata$X$cont,
    scale_dv = TRUE,
    scale_ivs = TRUE,
    s0_perc = NULL
)

# Scaling of X matters
testthat::expect_false(all((thrs_NOR - thrs_NOR_scaled_X) < tol))
testthat::expect_false(all((thrs_NOR - thrs_NOR_scaled_X_sacled_y) < tol))

# Scaling of y doe not matter!
testthat::expect_true(all((thrs_NOR - thrs_NOR_sacled_y) < tol))