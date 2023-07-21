# Project:   gspcr
# Objective: Test cp_AIC function
# Author:    Edoardo Costantini
# Created:   2023-04-18
# Modified:  2023-04-18
# Notes: 

# Define tolerance for differences
tol <- 1e-15

# Test: output class -----------------------------------------------------------

# Fit some model
lm_out <- lm(mpg ~ cyl + disp, data = mtcars)

# Compute AIC with your function
AIC_M <- cp_AIC(
    ll = logLik(lm_out),
    k = length(coef(lm_out)) + 1 # intercept + reg coefs + error variance
)

# Atomic numeric vector
testthat::expect_true(is.numeric(AIC_M))

# Length 1
testthat::expect_true(length(AIC_M) == 1)

# Test: manual computation = stats::AIC output ---------------------------------

# Compute AIC with R function
AIC_R <- stats::AIC(lm_out)

# R equal to manual
testthat::expect_true(AIC_R - AIC_M < tol)
