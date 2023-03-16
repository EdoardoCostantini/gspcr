# Project:   gspcr
# Objective: Testing the likelihood functions
# Author:    Edoardo Costantini
# Created:   2023-03-16
# Modified:  2023-03-16
# Notes: 

# Is the likelihood function working as expected? ------------------------------

# Fit a linear model
lm1 <- lm(mpg ~ cyl + disp, data = mtcars)

# Store sample size
n <- nobs(lm1)

# Compute the residuals
resi <- resid(lm1)

# Compute the ML residuals error variance
resi_var <- var(resi) * (n - 1) / n

# Store the likelihood value you compute
myLogLik <- loglike_norm(
  r = resid(lm1),
  s = sqrt(resi_var)
)

# Store the value produced by standard formula
statsLogLik <- as.numeric(logLik(lm1))

# Compare the values
testthat::expect_equal(myLogLik, statsLogLik)
