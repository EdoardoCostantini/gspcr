# Project:   gspcr
# Objective: Testing the compute_sc function
# Author:    Edoardo Costantini
# Created:   2023-04-11
# Modified:  2023-11-21
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
tol <- 1e-5

# Test: linear models ----------------------------------------------------------

# Linear model with lm and glm
lm_out <- lm(mpg ~ cyl + disp, data = mtcars_fact)
glm_out1 <- stats::glm(mpg ~ cyl, data = mtcars, family = "gaussian")
glm_out2 <- stats::glm(mpg ~ cyl + disp, data = mtcars, family = "gaussian")

# Get systematic components
sc_lm_out <- predict(lm_out, newdata = mtcars_fact[new_data, ])
sc_glm_out1 <- predict(glm_out1, newdata = mtcars_fact[new_data, ], type = "link")
sc_glm_out2 <- predict(glm_out2, newdata = mtcars_fact[new_data, ], type = "link")

# Use the function for linear model with lm and GLM
sc_fun_lm_out <- compute_sc(
    mod = lm_out,
    predictors = mtcars_fact[new_data, c("cyl", "disp")]
)

# Use the function with GLM (and a single predictor)
sc_fun_glm_out1 <- compute_sc(
    mod = glm_out1,
    predictors = mtcars_fact[new_data, "cyl", drop = FALSE]
)
# Use the function with GLM
sc_fun_glm_out2 <- compute_sc(
    mod = glm_out2,
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
testthat::expect_true(sum(sc_glm_out2 - sc_fun_glm_out2) < tol)

# Test: logistic regression models ---------------------------------------------

# Logistic regression
glm_logistic <- stats::glm(
    formula = am ~ cyl + disp,
    data = mtcars,
    family = "binomial"
)

# Get systematic components
sc_glm_logistic <- predict(
    object = glm_logistic,
    newdata = mtcars_fact[new_data, ],
    type = "link"
)

# Use the function for logistic regression
sc_fun_glm_logistic <- compute_sc(
    mod = glm_logistic,
    predictors = mtcars_fact[new_data, c("cyl", "disp")]
)

# Test the function returns a matrix
testthat::expect_true(is.matrix(sc_fun_glm_logistic))

# Test the matrix dimensionality is correct
testthat::expect_true(ncol(sc_fun_glm_logistic) == 1)

# Test the function is computing what is expected
testthat::expect_true(sum(sc_glm_logistic - sc_fun_glm_logistic) < tol)

# Test: baseline-category logistic regression ----------------------------------

# Baseline-category logistic regression
multi_out <- nnet::multinom(
    formula = gear ~ cyl + disp,
    data = mtcars_fact,
    trace = FALSE
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

# Test: proportional odds model ------------------------------------------------

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

# Test the function returns a matrix
testthat::expect_true(is.matrix(abx))

# Test the matrix dimensionality is correct
testthat::expect_true(ncol(abx) == (nlevels(mtcars_fact$carb) - 1))

# Test the function is computing what is expected
testthat::expect_true(ll_polr - logLik(plor_test) < tol)

# Test: Proportional odds model with a single predictor ------------------------

# Fit a logistic or probit regression model to an ordered factor response.
plor_test <- MASS::polr(
    formula = carb ~ cyl,
    data = mtcars_fact,
    method = "logistic" # proportional odds logistic regression
)

# Get systematic component
abx_single <- compute_sc(
    mod = plor_test,
    predictors = mtcars_fact[, "cyl", drop = FALSE]
)

# Test the function returns a matrix
testthat::expect_true(is.matrix(abx_single))

# Test the matrix dimensionality is correct
testthat::expect_true(ncol(abx_single) == (nlevels(mtcars_fact$carb) - 1))

# Test: poisson regression -----------------------------------------------------

# Fit the model
glm_poisson <- glm(
    formula = carb ~ cyl + disp,
    data = mtcars,
    family = "poisson"
)

# Get systematic component with GLM function
sc_glm_poisson <- predict(
    object = glm_poisson,
    newdata = mtcars[new_data, ],
    type = "link"
)

# Get systematic component with custom function
sc_poisson <- compute_sc(
    mod = glm_poisson,
    predictors = mtcars[new_data, c("cyl", "disp"), drop = FALSE]
)

# Test the function returns a matrix
testthat::expect_true(is.matrix(sc_poisson))

# Test the matrix dimensionality is correct
testthat::expect_true(ncol(sc_poisson) == 1)

# Test the function is computing what is expected
testthat::expect_true(sum(sc_poisson - sc_glm_poisson) < tol)

# Test: null models ------------------------------------------------------------

# Null models
null_lm <- lm(mpg ~ 1, data = mtcars_fact)
null_binlo <- stats::glm(am ~ 1, data = mtcars, family = "binomial")
null_multi <- nnet::multinom(formula = gear ~ 1, data = mtcars_fact, trace = FALSE)
null_polr <- MASS::polr(carb ~ 1, mtcars_fact, method = "logistic")

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