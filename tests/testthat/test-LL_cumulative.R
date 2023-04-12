# Project:   gspcr
# Objective: Test log-likelihood for proportional odds logistic regression (ordinal variables)
# Author:    Edoardo Costantini
# Created:   2023-04-04
# Modified:  2023-04-04
# Notes: 

# Correct value? ---------------------------------------------------------------

# Create a copy of the data
mtcars_fact <- mtcars

# Make carb a factor
mtcars_fact$carb <- factor(mtcars_fact$carb, ordered = TRUE)

# Count the number of levels
J <- nlevels(mtcars_fact$carb)

# Fit a logistic or probit regression model to an ordered factor response.
plor_test <- MASS::polr(
    formula = carb ~ disp + hp,
    data = mtcars_fact,
    method = "logistic" # proportional odds logistic regression
)

# LogLikelihood w/ R
ll_R <- as.numeric(logLik(plor_test))

# Compute bx
bx <- as.matrix(mtcars[, c("disp", "hp")]) %*% plor_test$coefficients

# Compute aj + bx
abx <- sapply(plor_test$zeta, function(a) {
    a - bx
})

# Use the function
ll_fun <- LL_cumulative(
    y = mtcars_fact$carb,
    syst_comp = abx
)

# Define tolerance for difference
tol <- 1e-15

# Check the values are all the same
testthat::expect_true(ll_fun - ll_R < tol)
