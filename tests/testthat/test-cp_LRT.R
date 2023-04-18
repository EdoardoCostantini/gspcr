# Project:   gspcr
# Objective: Test cp_LRT function
# Author:    Edoardo Costantini
# Created:   2023-04-18
# Modified:  2023-04-18
# Notes: 

# Define tolerance for differences
tol <- 1e-15

# Test: output class -----------------------------------------------------------

# Fit a nested model
nested <- glm(mpg ~ cyl + disp, data = mtcars)

# Fit a complex model
complex <- glm(mpg ~ cyl + disp + hp + am, data = mtcars)

# Compute log-likelihood statistic with your function
LRT_M <- cp_LRT(
    ll_restricted = logLik(nested),
    ll_full = logLik(complex)
)

# Atomic numeric vector
testthat::expect_true(is.numeric(LRT_M))

# Length 1
testthat::expect_true(length(LRT_M) == 1)

# Test: manual computation = lmtest::lrtest output -----------------------------

# Likelihood ratio test
LRT_test <- lmtest::lrtest(nested, complex)

# Extract the LRT value
LRT_R <- LRT_test$Chisq[2]

# R equal to manual
testthat::expect_true(LRT_R - LRT_M < tol)


