# Project:   gspcr
# Objective: Test log-likelihood for the baseline category logistic regression (nominal variables)
# Author:    Edoardo Costantini
# Created:   2023-04-05
# Modified:  2023-04-05
# Notes: 

# Correct value? ---------------------------------------------------------------

# Create a copy of the data
mtcars_fact <- mtcars

# Make gear a factor
mtcars_fact$gear <- factor(mtcars_fact$gear)

# Fit a baseline category logistic regression model to a factor response.
out <- nnet::multinom(
    formula = gear ~ disp,
    data = mtcars_fact
)

# LogLikelihood w/ R
ll_R <- as.numeric(logLik(out))

# Create multinomial trial representation of the dv
y <- FactoMineR::tab.disjonctif(mtcars_fact$gear)

# Create the systematic component
syst_comp <- cbind(
    int = 1,
    as.matrix(mtcars[, "disp", drop = FALSE])
) %*% t(coef(out))

# Input
ll_fun <- LL_baseline(
    y = y,
    syst_comp = syst_comp
)

# Define tolerance for difference
tol <- 1e-10

# Check the values are all the same
testthat::expect_true(ll_R - ll_fun < tol)

# Factor input -----------------------------------------------------------------

# Compute ll with function using a factor as input
ll_fun_fact <- LL_baseline(
    y = mtcars_fact$gear,
    syst_comp = syst_comp
)

# Check the values are all the same
testthat::expect_true(ll_R - ll_fun_fact < tol)