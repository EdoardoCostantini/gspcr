# Project:   gspcr
# Objective: Test cp_BIC function
# Author:    Edoardo Costantini
# Created:   2023-04-18
# Modified:  2023-04-18
# Notes: 

# Define tolerance for differences
tol <- 1e-5

# Test: output class -----------------------------------------------------------

# Fit some model
lm_out <- lm(mpg ~ cyl + disp, data = mtcars)

# Compute BIC with your function
BIC_M <- cp_BIC(
    ll = logLik(lm_out),
    n = nobs(lm_out),
    k = length(coef(lm_out)) + 1 # intercept + reg coefs + error variance
)

# Atomic numeric vector
testthat::expect_true(is.numeric(BIC_M))

# Length 1
testthat::expect_true(length(BIC_M) == 1)

# Test: manual computation = stats::BIC output ---------------------------------

# Compute BIC with R function
BIC_R <- stats::BIC(lm_out)

# R equal to manual
testthat::expect_true(BIC_R - BIC_M < tol)
