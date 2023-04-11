# Project:   gspcr
# Objective: Testing the likelihood functions
# Author:    Edoardo Costantini
# Created:   2023-03-31
# Modified:  2023-04-04
# Notes:

# Correct value? ---------------------------------------------------------------

# Create a copy of the data
mtcars_fact <- mtcars

# Fit a linear model
glm1 <- stats::glm(am ~ cyl + disp, data = mtcars, family = "binomial")

# Extract important objects
y <- mtcars$am
n <- length(y)
p <- predict(glm1, type = "response")
lgt <- predict(glm1, type = "link") # logit values

# LogLikelihood w/ R
ll_R <- as.numeric(logLik(glm1))

# LogLikelihood w/ manual easy way
ll_man1 <- log(prod((p)^y * (1 - p)^(1-y)))

# LogLikelihood w/ manual easy way
ll_man2 <- sum(y * lgt - log(1 + exp(lgt)))

# Active predictors
active_set <- names(stats::coef(glm1)[-1])

# Obtain a linear predictor on new data
syst_comp <- cbind(int = 1, as.matrix(mtcars[, active_set])) %*% stats::coef(glm1)

# With function
ll_fun <- LL_binomial(
    y = mtcars$am,
    lgt = syst_comp
)

# Collect values
lls <- c(ll_R, ll_man1, ll_man2, ll_fun)

# Define tolerance for difference
tol <- 1e-10

# Check the values are all the same
testthat::expect_true(max(lls) - min(lls) < tol)

# Factor input -----------------------------------------------------------------

# Transform dv to factor
mtcars_fact$am <- factor(
    x = mtcars$am,
    levels = c(0, 1),
    labels = c("automatic", "manual")
)

# Factor input
ll_fun_fact <- LL_binomial(
    y = mtcars_fact$am,
    lgt = syst_comp
)

# Collect values
lls <- c(ll_fun, ll_fun_fact)

# Check the values are all the same
testthat::expect_true(max(lls) - min(lls) < tol)

# Works for a null model -------------------------------------------------------

# Fit a linear model
glm0 <- stats::glm(am ~ 1, data = mtcars, family = "binomial")

# LogLikelihood w/ R
ll_R <- as.numeric(logLik(glm0))

# With function
ll_fun <- LL_binomial(
    y = mtcars$am,
    lgt = predict(glm0, type = "link") # logit values
)

# Check the values are all the same
testthat::expect_true(ll_R - ll_fun < tol)