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

# Make carb an ordered factor
mtcars_fact$carb <- factor(mtcars_fact$carb, ordered = TRUE)

# New data index
new_data <- 1:10

# Define a tolerance for differences in testing
tol <- 1e-15

# Test function works with null models -----------------------------------------

# Null models
null_lm <- lm(mpg ~ 1, data = mtcars_fact)
null_binlo <- stats::glm(am ~ 1, data = mtcars, family = "binomial")
null_multi <- nnet::multinom(formula = gear ~ 1, data = mtcars_fact)
null_polr <- MASS::polr(
    formula = carb ~ 1,
    data = mtcars_fact,
    method = "logistic" # proportional odds logistic regression
)

# Testing data for null models
null_data <- matrix(1, nrow = 10, ncol = 1)

# Get systematic components
sc_lm_out_null <- predict(null_lm, newdata = mtcars_fact[new_data, ])

# Use the function
null_lm_sc <- compute_sc(
    mod = null_lm,
    predictors = null_data
)
null_binlo_sc <- compute_sc(
    mod = null_binlo,
    predictors = null_data
)
null_multi_sc <- compute_sc(
    mod = null_multi,
    predictors = null_data
)
null_polr_sc <- compute_sc(
    mod = null_polr,
    predictors = null_data
)

# Test the function returns a matrix
testthat::expect_true(is.matrix(null_lm_sc))
testthat::expect_true(is.matrix(null_binlo_sc))
testthat::expect_true(is.matrix(null_multi_sc))
testthat::expect_true(is.matrix(null_polr_sc))

# Test the matrix dimensionality is correct
testthat::expect_true(ncol(null_lm_sc) == 1)
testthat::expect_true(ncol(null_binlo_sc) == 1)
testthat::expect_true(ncol(null_multi_sc) == nlevels(mtcars_fact$gear) - 1)
testthat::expect_true(ncol(null_polr_sc) == nlevels(mtcars_fact$carb) - 1)

# Test function works with linear models ---------------------------------------

# Linear model with lm and glm
lm_out <- lm(mpg ~ cyl + disp, data = mtcars_fact)
glm_out1 <- stats::glm(mpg ~ cyl + disp, data = mtcars, family = "gaussian")

# Get systematic components
sc_lm_out <- predict(lm_out, newdata = mtcars_fact[new_data, ])
sc_glm_out1 <- predict(glm_out1, newdata = mtcars_fact[new_data, ], type = "link")

# Use the function for linear model with lm and GLM
sc_fun_lm_out <- compute_sc(
    mod = lm_out,
    predictors = mtcars_fact[new_data, c("cyl", "disp")]
)
sc_fun_glm_out1 <- compute_sc(
    mod = glm_out1,
    predictors = mtcars_fact[new_data, c("cyl", "disp")]
)

# Test the function returns a matrix
testthat::expect_true(is.matrix(sc_fun_lm_out))
testthat::expect_true(is.matrix(sc_fun_glm_out1))

# Test the matrix dimensionality is correct
testthat::expect_true(ncol(sc_fun_lm_out) == 1)
testthat::expect_true(ncol(sc_fun_glm_out1) == 1)

# Test the function is computing what is expected
testthat::expect_true(sum(sc_lm_out - sc_fun_lm_out) < tol)
testthat::expect_true(sum(sc_glm_out1 - sc_fun_glm_out1) < tol)

# Test function works with logistic regression models --------------------------

# Logistic regression
glm_out2 <- stats::glm(am ~ cyl + disp, data = mtcars, family = "binomial")

# Get systematic components
sc_glm_out2 <- predict(glm_out2, newdata = mtcars_fact[new_data, ], type = "link")

# Use the function for logistic regression
sc_fun_glm_out2 <- compute_sc(
    mod = glm_out2,
    predictors = mtcars_fact[new_data, c("cyl", "disp")]
)

# Test the function returns a matrix
testthat::expect_true(is.matrix(sc_fun_glm_out2))

# Test the matrix dimensionality is correct
testthat::expect_true(ncol(sc_fun_glm_out2) == 1)

# Test the function is computing what is expected
testthat::expect_true(sum(sc_glm_out2 - sc_fun_glm_out2) < tol)

# Test function works with baseline-category logistic regression ---------------

# Baseline-category logistic regression
multi_out <- nnet::multinom(
    formula = gear ~ cyl + disp,
    data = mtcars_fact
)

# Get systematic components
multi_out_probs <- predict(
    multi_out,
    newdata = mtcars_fact[new_data, ], 
    type = "probs"
)

# Use the function for Baseline-category logistic regression
sc_fun_multi_out <- compute_sc(
    mod = multi_out,
    predictors = mtcars_fact[new_data, c("cyl", "disp")]
)

# Compute the logits for the baseline category
logits <- cbind(0, sc_fun_multi_out)

# Transform to probabilities
multi_out_probs_fun <- exp(logits) / rowSums(exp(logits))

# Test the function returns a matrix
testthat::expect_true(is.matrix(sc_fun_multi_out))

# Test the matrix dimensionality is correct
testthat::expect_true(ncol(sc_fun_multi_out) == (nlevels(mtcars_fact$gear) - 1))

# Test the function is computing what is expected
testthat::expect_true(sum(multi_out_probs - multi_out_probs_fun) < tol)

# Test function works with proportional odds model -----------------------------

# Fit a logistic or probit regression model to an ordered factor response.
plor_test <- MASS::polr(
    formula = carb ~ cyl + disp,
    data = mtcars_fact,
    method = "logistic" # proportional odds logistic regression
)

# Get systematic component
abx <- compute_sc(
    mod = plor_test,
    predictors = mtcars_fact[, c("cyl", "disp")]
)

# Transform into cumulative probabilities
cumsum_man <- cbind(0, exp(abx) / (1 + exp(abx)), 1)

# Create multinomial trial representation of the dv
y <- FactoMineR::tab.disjonctif(mtcars_fact$carb)

# Define a storing matrix
shelf <- matrix(nrow = nrow(cumsum_man), ncol = ncol(cumsum_man)-1)

# Then I can use this transformation into the computation of the log-likelihood
for (i in 1:nrow(cumsum_man)) {
    # i <- 1
    for (j in 2:ncol(cumsum_man)) {
        # j <- 2
        shelf[i, j - 1] <- y[i, j - 1] * log(cumsum_man[i, j] - cumsum_man[i, j - 1])
    }
}

# And then I can compute the log-likelihood
ll_polr <- sum(sum(shelf))

logLik(plor_test)

# Test the function returns a matrix
testthat::expect_true(is.matrix(abx))

# Test the matrix dimensionality is correct
testthat::expect_true(ncol(abx) == (nlevels(mtcars_fact$carb) - 1))

# Test the function is computing what is expected
testthat::expect_true(ll_polr - logLik(plor_test) < tol)
