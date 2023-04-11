# Project:   gspcr
# Objective: Testing the compute_sc function
# Author:    Edoardo Costantini
# Created:   2023-04-11
# Modified:  2023-04-11
# Notes: 

# Prepare the data -------------------------------------------------------------

# Create a copy of the data
mtcars_fact <- mtcars

# Make am a factor
mtcars_fact$am <- factor(
    x = mtcars$am,
    levels = c(0, 1),
    labels = c("automatic", "manual")
)

# Make gear a factor
mtcars_fact$gear <- factor(mtcars_fact$gear)

# Fit the models ---------------------------------------------------------------

# Linear model with lm (null)
lm_out_null <- lm(mpg ~ 1, data = mtcars_fact)

# Linear model with lm
lm_out <- lm(mpg ~ cyl + disp, data = mtcars_fact)

# Linear model with GLM
glm_out1 <- stats::glm(mpg ~ cyl + disp, data = mtcars, family = "gaussian")

# Logistic regression
glm_out2 <- stats::glm(am ~ cyl + disp, data = mtcars, family = "binomial")

# Baseline-category logistic regression
multi_out <- out <- nnet::multinom(
    formula = gear ~ cyl + disp,
    data = mtcars_fact
)

# Get systematic components with default approach ------------------------------

sc_lm_out_null <- predict(lm_out_null, newdata = mtcars_fact[1:10, ])
sc_lm_out <- predict(lm_out, newdata = mtcars_fact[1:10, ])
sc_glm_out1 <- predict(glm_out1, newdata = mtcars_fact[1:10, ], type = "link")
sc_glm_out2 <- predict(glm_out2, newdata = mtcars_fact[1:10, ], type = "link")
multi_out_probs <- predict(multi_out, newdata = mtcars_fact[1:10, ], type = "probs")

# Use the function -------------------------------------------------------------

# Linear model with lm (null)
sc_fun_lm_out_null <- compute_sc(
    mod = lm_out_null,
    predictors = matrix(1, nrow = 10, ncol = 1)
)

# Linear model with lm
sc_fun_lm_out <- compute_sc(
    mod = lm_out,
    predictors = mtcars_fact[1:10, c("cyl", "disp")]
)

# Linear model with GLM
sc_fun_glm_out1 <- compute_sc(
    mod = glm_out1,
    predictors = mtcars_fact[1:10, c("cyl", "disp")]
)

# Logistic regression
sc_fun_glm_out2 <- compute_sc(
    mod = glm_out2,
    predictors = mtcars_fact[1:10, c("cyl", "disp")]
)

# Baseline-category logistic regression
sc_fun_multi_out <- compute_sc(
    mod = multi_out,
    predictors = mtcars_fact[1:10, c("cyl", "disp")]
)

# Compute the logits for the baseline category
logits <- cbind(0, sc_fun_multi_out)

# Transform to probabilities
multi_out_probs_fun <- exp(logits) / rowSums(exp(logits))

# Test function ----------------------------------------------------------------

# Define a tolerance
tol <- 1e-15

# Test the function returns a matrix
testthat::expect_true(is.matrix(sc_fun_lm_out_null))
testthat::expect_true(is.matrix(sc_fun_lm_out))
testthat::expect_true(is.matrix(sc_fun_glm_out1))
testthat::expect_true(is.matrix(sc_fun_glm_out2))
testthat::expect_true(is.matrix(sc_fun_multi_out))

# Test the matrix dimensionality is correct
testthat::expect_true(ncol(sc_fun_lm_out_null) == 1)
testthat::expect_true(ncol(sc_fun_lm_out) == 1)
testthat::expect_true(ncol(sc_fun_glm_out1) == 1)
testthat::expect_true(ncol(sc_fun_glm_out2) == 1)
testthat::expect_true(ncol(sc_fun_multi_out) == (nlevels(mtcars_fact$gear) - 1))

# Test the function is computing what it's expected
testthat::expect_true(sum(sc_lm_out_null - sc_fun_lm_out_null) < tol)
testthat::expect_true(sum(sc_lm_out - sc_fun_lm_out) < tol)
testthat::expect_true(sum(sc_glm_out1 - sc_fun_glm_out1) < tol)
testthat::expect_true(sum(sc_glm_out2 - sc_fun_glm_out2) < tol)
testthat::expect_true(sum(multi_out_probs - multi_out_probs_fun) < tol)