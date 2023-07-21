# Project:   gspcr
# Objective: Test the cp_validation_fit function
# Author:    Edoardo Costantini
# Created:   2023-05-30
# Modified:  2023-05-30
# Notes: 

# Define tolerance
tol <- 1e-5

# Test: Correct results when train = valid -------------------------------------

# Use the function with training = validation data
fit_out <- cp_validation_fit(
    y_train = as.matrix(mtcars[, 1]),
    y_valid = as.matrix(mtcars[, 1]),
    X_train = as.matrix(mtcars[, -1]),
    X_valid = as.matrix(mtcars[, -1]),
    fam = "gaussian",
    fit_measure = "F"
)

# Fit the expected model
mod_R <- lm(mpg ~ ., data = mtcars)

# Extract F statistic
f_lm <- summary(mod_R)$fstatistic["value"]

# Test F statistic is the same
testthat::expect_true((f_lm - fit_out) < tol)