# Project:   gspcr
# Objective: Test log-likelihood for the baseline category logistic regression (nominal variables)
# Author:    Edoardo Costantini
# Created:   2023-04-05
# Modified:  2023-04-13
# Notes: 

# Define tolerance for differences
tol <- 1e-10

# Test: Correct result ---------------------------------------------------------

# Create a copy of the data
mtcars_fact <- mtcars

# Make gear a factor
mtcars_fact$gear <- factor(mtcars_fact$gear)

# Fit a baseline category logistic regression model to a factor response.
glm_baseline <- nnet::multinom(
    formula = gear ~ disp,
    data = mtcars_fact
)

# LogLikelihood w/ R
ll_R <- as.numeric(logLik(glm_baseline))

# Input
out <- LL_baseline(
    y = mtcars_fact$gear,
    x = mtcars_fact[, "disp", drop = FALSE],
    mod = glm_baseline
)

# Check the values are all the same
testthat::expect_true(ll_R - out$ll < tol)

# Test: Matrix input -----------------------------------------------------------

# Create matrix version of DV
y <- FactoMineR::tab.disjonctif(mtcars_fact$gear)

# Compute ll with function using a factor as input
out_fact <- LL_baseline(
    y = y,
    x = mtcars_fact[, "disp", drop = FALSE],
    mod = glm_baseline
)

# Check the values are all the same
testthat::expect_true(ll_R - out_fact$ll < tol)

# Test: Character input --------------------------------------------------------

# Create matrix version of DV
y <- as.character(mtcars_fact$gear)

# Compute ll with function using a factor as input
out_charct <- LL_baseline(
    y = y,
    x = mtcars_fact[, "disp", drop = FALSE],
    mod = glm_baseline
)

# Check the values are all the same
testthat::expect_true(ll_R - out_charct$ll < tol)

# Test: Numeric input --------------------------------------------------------

# Create matrix version of DV
y <- as.numeric(mtcars_fact$gear)

# Compute ll with function using a factor as input
out_numeric <- LL_baseline(
    y = y,
    x = mtcars_fact[, "disp", drop = FALSE],
    mod = glm_baseline
)

# Check the values are all the same
testthat::expect_true(ll_R - out_numeric$ll < tol)