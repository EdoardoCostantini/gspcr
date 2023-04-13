# Project:   gspcr
# Objective: Testing the likelihood functions
# Author:    Edoardo Costantini
# Created:   2023-03-31
# Modified:  2023-04-13
# Notes:

# Define tolerance for difference
tol <- 1e-10

# Test: Correct result ---------------------------------------------------------

# Create a copy of the data
mtcars_fact <- mtcars

# Fit a linear model
glm_logistic <- stats::glm(am ~ cyl + disp, data = mtcars, family = "binomial")

# LogLikelihood w/ R
ll_R <- as.numeric(logLik(glm_logistic))

# With function
out <- LL_binomial(
    y = mtcars$am,
    x = mtcars[, c("cyl", "disp")],
    mod = glm_logistic
)

# Define tolerance for difference
tol <- 1e-10

# Check the values are all the same
testthat::expect_true(ll_R - out$ll < tol)

# Test: Factor input -----------------------------------------------------------

# Transform dv to factor
mtcars_fact$am <- factor(
    x = mtcars$am,
    levels = c(0, 1),
    labels = c("automatic", "manual")
)

# Fit a linear model
glm_logistic <- stats::glm(
    formula = am ~ cyl + disp,
    data = mtcars_fact,
    family = "binomial"
)

# LogLikelihood w/ R
ll_R <- as.numeric(logLik(glm_logistic))

# Factor input
out <- LL_binomial(
    y = mtcars_fact$am,
    x = mtcars_fact[, c("cyl", "disp")],
    mod = glm_logistic
)

# Check the values are all the same
testthat::expect_true(ll_R - out$ll < tol)

# Test: Null model -------------------------------------------------------------

# Fit a linear model
glm_logistic_null <- stats::glm(am ~ 1, data = mtcars, family = "binomial")

# LogLikelihood w/ R
ll_R <- as.numeric(logLik(glm_logistic_null))

# With function
out <- LL_binomial(
    y = mtcars_fact$am,
    x = matrix(1, nrow = length(mtcars_fact$am), ncol = 1),
    mod = glm_logistic_null
)

# Check the values are all the same
testthat::expect_true(ll_R - out$ll < tol)