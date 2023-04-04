# Project:   gspcr
# Objective: Testing the likelihood functions
# Author:    Edoardo Costantini
# Created:   2023-03-16
# Modified:  2023-04-04
# Notes: 

# Correct value? ---------------------------------------------------------------

# Fit a linear model
lm1 <- lm(mpg ~ cyl + disp, data = mtcars)

# Store sample size
n <- nobs(lm1)

# Compute the residuals
resi <- resid(lm1)

# Compute the ML residuals error variance
resi_var <- var(resi) * (n - 1) / n

# With function
ll_fun <- LL_gaussian(
  y = mtcars$mpg,
  y_hat = predict(lm1),
  mod = lm1
)

# Store the value produced by the standard R function
ll_R <- as.numeric(logLik(lm1))

# Define tolerance for difference
tol <- 1e-10

# Check the values are all the same
testthat::expect_true(ll_R - ll_fun < tol)