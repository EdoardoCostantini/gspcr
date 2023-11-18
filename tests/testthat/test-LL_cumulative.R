# Project:   gspcr
# Objective: Test log-likelihood for proportional odds logistic regression (ordinal variables)
# Author:    Edoardo Costantini
# Created:   2023-04-04
# Modified:  2023-04-13
# Notes:

# Define tolerance for difference
tol <- 1e-5

# Test: Correct result ---------------------------------------------------------

# Create a copy of the data
mtcars_fact <- mtcars

# Make carb a factor
mtcars_fact$carb <- factor(mtcars_fact$carb, ordered = TRUE)

# Fit a logistic or probit regression model to an ordered factor response.
glm_polr <- MASS::polr(
    formula = carb ~ disp + hp,
    data = mtcars_fact,
    method = "logistic" # proportional odds logistic regression
)

# LogLikelihood w/ R
ll_R <- as.numeric(logLik(glm_polr))

# Use the function
out <- LL_cumulative(
    y = mtcars_fact$carb,
    x = mtcars[, c("disp", "hp")],
    mod = glm_polr
)

# Check the values are all the same
testthat::expect_true(ll_R - out$ll < tol)

# Test: Matrix input -----------------------------------------------------------

# Create matrix version of DV
y <- FactoMineR::tab.disjonctif(mtcars_fact$carb)

# Use the function
out <- LL_cumulative(
    y = y,
    x = mtcars[, c("disp", "hp")],
    mod = glm_polr
)

# Check the values are all the same
testthat::expect_true(ll_R - out$ll < tol)
