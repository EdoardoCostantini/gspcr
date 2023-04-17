# Project:   gspcr
# Objective: Test cp_gR2 function
# Author:    Edoardo Costantini
# Created:   2023-04-14
# Modified:  2023-04-14
# Notes: 

# Define tolerance for differences
tol <- 1e-15

# Test: expected output --------------------------------------------------------

# Fit a null model
lm_n <- lm(mpg ~ 1, data = mtcars)

# Fit a full model
lm_f <- lm(mpg ~ cyl + disp, data = mtcars)

# Compute generalized R2
gr2 <- cp_gR2(
    ll_n = as.numeric(logLik(lm_n)),
    ll_f = as.numeric(logLik(lm_f)),
    n = nobs(lm_f)
)

# Atomic numeric vector
testthat::expect_true(is.numeric(gr2))

# Length 1
testthat::expect_true(length(gr2) == 1)

# Test: generalized R2 = R2 for linear regression ------------------------------

# Extract regular R2
r2 <- summary(lm_f)$r.squared

# gr2 equal to R2
testthat::expect_true(r2 - gr2 < tol)

# Test: Works for GLMs ---------------------------------------------------------

# Fit a null model
glm_n <- glm(am ~ 1, data = mtcars, family = "binomial")

# Fit a full model
glm_f <- glm(am ~ cyl + disp, data = mtcars, family = "binomial")

# Extract the model likelihood
ll_n <- as.numeric(logLik(glm_n))
ll_f <- as.numeric(logLik(glm_f))

# Compute generalized R2
gr2 <- cp_gR2(
    ll_n = ll_n,
    ll_f = ll_f,
    n = nobs(glm_f)
)

# Compute maximum gR2 value
gr2_max <- 1 - exp(ll_n)^(2 / nobs(glm_f))

# Positive
testthat::expect_true(gr2 > 0)

# Smaller than the theoretical maximum
testthat::expect_true(gr2 < gr2_max)