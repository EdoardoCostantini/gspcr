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

# Test: handles duplicated choice values ---------------------------------------

# Create a matrix of (uneven) scores
set.seed(1234)
scor <- matrix(rnorm(9), ncol = 3, nrow = 3)
scor_lwr <- matrix(NA, ncol = 3, nrow = 3)
scor_lwr <- matrix(NA, ncol = 3, nrow = 3)

# Define the location of the choice
max_location <- which(scor == max(scor), arr.ind = TRUE)

# Vertical same value
scor_2_sol_vert <- scor
scor_2_sol_vert[2, 1] <- scor[max_location]

# Apply function
cv_choose.out_vert <- cv_choose(
  scor = scor_2_sol_vert,
  scor_lwr = scor_lwr,
  scor_upr = scor_lwr,
  K = K,
  fit_measure = "F"
)

# Check the solution with fewer PCs is selected
testthat::expect_equal(
  as.vector(cv_choose.out_vert$default),
  c(2, 1)
)

# Horizontal same value
scor_2_sol_hori <- scor
scor_2_sol_hori[3, 2] <- scor[max_location]

# Apply function
cv_choose.out_hori <- cv_choose(
  scor = scor_2_sol_hori,
  scor_lwr = scor_lwr,
  scor_upr = scor_lwr,
  K = K,
  fit_measure = "F"
)

# Check the solution with fewer predictors involved is selected
testthat::expect_equal(
  as.vector(cv_choose.out_hori$default),
  c(3, 2)
)

# Mixed same value
scor_2_sol_mix <- scor
scor_2_sol_mix[3, 2] <- scor[max_location]
scor_2_sol_mix[2, 1] <- scor[max_location]
scor_2_sol_mix[2, 3] <- scor[max_location]
scor_2_sol_mix[1, 3] <- scor[max_location]

# Apply function
cv_choose.out_mix <- cv_choose(
  scor = scor_2_sol_mix,
  scor_lwr = scor_lwr,
  scor_upr = scor_lwr,
  K = K,
  fit_measure = "F"
)

# Check the solution with fewer predictors involved is selected
testthat::expect_equal(
  as.vector(cv_choose.out_mix$default),
  c(1, 3)
)