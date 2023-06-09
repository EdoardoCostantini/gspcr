# Project:   gspcr
# Objective: Testing the likelihood functions
# Author:    Edoardo Costantini
# Created:   2023-03-16
# Modified:  2023-04-13
# Notes: 

# Setup: Define example inputs with known outcome ------------------------------

# Score matrices
scor <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
scor_lwr <- matrix(c(1, 2, 3, 4, 5, 6) - 1.5, nrow = 3, ncol = 2)
scor_upr <- matrix(c(1, 2, 3, 4, 5, 6) + 1.5, nrow = 3, ncol = 2)

# Number of folds
K <- 10

# Test: Returns the highest score when needed (e.g., F) ------------------------

# Type of fit_measure
fit_measure <- "F"

# Apply function
cv_choose.out <- cv_choose(
  scor = scor,
  scor_lwr = scor_lwr,
  scor_upr = scor_upr,
  K = K,
  fit_measure = fit_measure
)

# Check default results for an F fit_measure
testthat::expect_equal(as.vector(cv_choose.out$default), c(3, 2))

# Check 1SE results for an F fit_measure
testthat::expect_equal(as.vector(cv_choose.out$oneSE), c(2, 2))

# Test: Returns the lowest score when needed (e.g., MSE) -----------------------

# Type of fit_measure
fit_measure <- "MSE"

# Apply function
cv_choose.out <- cv_choose(
  scor = scor,
  scor_lwr = scor_lwr,
  scor_upr = scor_upr,
  K = K,
  fit_measure = fit_measure
)

# Check default results for an F fit_measure
testthat::expect_equal(as.vector(cv_choose.out$default), c(1, 1))

# Check 1SE results for an F fit_measure
testthat::expect_equal(as.vector(cv_choose.out$oneSE), c(2, 1))

# ls(out_fit_meas[[3]])

# cv_choose(
#   scor = out_fit_meas[[3]]$scor,
#   scor_lwr = out_fit_meas[[3]]$scor_lwr,
#   scor_upr = out_fit_meas[[3]]$scor_upr,
#   K = 10,
#   fit_measure = "AIC"
# )
