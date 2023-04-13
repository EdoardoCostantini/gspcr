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

# Define y
y <- mtcars$carb

# Define x
x <- model.matrix( ~ ., mtcars[, c("disp", "hp")])

# Compute bx
bx <- x %*% glm_poisson$coefficients

# Use the function
ll_fun <- LL_poisson(
    y = mtcars$carb,
    syst_comp = bx
)

# Check the values are all the same
testthat::expect_true(ll_fun - ll_R < tol)
