# Project:   gspcr
# Objective: Testing the likelihood functions
# Author:    Edoardo Costantini
# Created:   2023-03-16
# Modified:  2023-03-16
# Notes: 

# Define example inputs with known outcome -------------------------------------

# Score matrices
scor <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
scor.lwr <- matrix(c(1, 2, 3, 4, 5, 6) - 1.5, nrow = 3, ncol = 2)
scor.upr <- matrix(c(1, 2, 3, 4, 5, 6) + 1.5, nrow = 3, ncol = 2)

# Number of folds
K <- 10

# High score (e.g., F) ---------------------------------------------------------

# Type of test
test <- "F"

# Apply function
cv_choose.out <- cv_choose(
  scor = scor,
  scor.lwr = scor.lwr,
  scor.upr = scor.upr,
  K = K,
  test = test
)

# Check default results for an F test
testthat::expect_equal(as.vector(cv_choose.out$default), c(3, 2))

# Check 1SE results for an F test
testthat::expect_equal(as.vector(cv_choose.out$oneSE), c(2, 2))

# Low score (e.g., MSE) --------------------------------------------------------

# Type of test
test <- "MSE"

# Apply function
cv_choose.out <- cv_choose(
  scor = scor,
  scor.lwr = scor.lwr,
  scor.upr = scor.upr,
  K = K,
  test = test
)

# Check default results for an F test
testthat::expect_equal(as.vector(cv_choose.out$default), c(1, 1))

# Check 1SE results for an F test
testthat::expect_equal(as.vector(cv_choose.out$oneSE), c(2, 1))