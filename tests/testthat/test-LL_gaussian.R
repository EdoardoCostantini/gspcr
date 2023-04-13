# Project:   gspcr
# Objective: Testing the likelihood functions
# Author:    Edoardo Costantini
# Created:   2023-03-16
# Modified:  2023-04-13
# Notes: 

# Define tolerance for difference
tol <- 1e-10

# Test: Correct result ---------------------------------------------------------

# Fit a linear model
glm_regression <- lm(mpg ~ disp + hp, data = mtcars)

# With function
out <- LL_gaussian(
  y = mtcars$mpg,
  x = mtcars[, c("disp", "hp")],
  mod = glm_regression
)

# Store the value produced by the standard R function
ll_R <- as.numeric(logLik(glm_regression))

# Check the values are all the same
testthat::expect_true(ll_R - out$ll < tol)