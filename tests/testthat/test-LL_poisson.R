# Project:   gspcr
# Objective: Testing the likelihood functions
# Author:    Edoardo Costantini
# Created:   2023-04-13
# Modified:  2023-04-13
# Notes: 

# Define tolerance for difference
tol <- 1e-10

# Test: Correct result ---------------------------------------------------------

# Fit the model
glm_poisson <- glm(
    formula = carb ~ disp + hp,
    data = mtcars,
    family = "poisson"
)

# LogLikelihood w/ R
ll_R <- as.numeric(logLik(glm_poisson))

# Use the function
out <- LL_poisson(
    y = mtcars$carb,
    x = mtcars[, c("disp", "hp")],
    mod = glm_poisson
)

# Check the values are all the same
testthat::expect_true(ll_R - out$ll < tol)
