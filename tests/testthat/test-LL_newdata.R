# Project:   gspcr
# Objective: Test the LL_newdata function
# Author:    Edoardo Costantini
# Created:   2023-04-04
# Modified:  2023-08-01
# Notes: 

# Test: Correct results when train = valid -------------------------------------

# Use the function with training = validation data
mod_out <- LL_newdata(
    y_train = as.matrix(mtcars[, 1]),
    y_valid = as.matrix(mtcars[, 1]),
    X_train = as.matrix(mtcars[, -1]),
    X_valid = as.matrix(mtcars[, -1]),
    fam = "gaussian"
)

# Fit the expected model
mod_R <- lm(mpg ~ ., data = mtcars)

# Obtain the predictions
fit_R <- predict(mod_R)

# Obtain the Log-likelihood
LL_R <- as.numeric(logLik(mod_R))

# Test model parameter estimates are as expected
# testthat::expect_equal(coef(mod_R), coef(mod_out$mod))

# Test predicted values are as expected
testthat::expect_true(sum(fit_R - mod_out$yhat_va) == 0)

# Test log-likelihood value is as expected
testthat::expect_equal(LL_R, mod_out$LL)

# Test: Correct results when train != valid ------------------------------------

# Example inputs
y_train <- as.matrix(mtcars[1:20, 1])
y_valid <- as.matrix(mtcars[-c(1:20), 1])
X_train <- as.matrix(mtcars[c(1:20), -1])
X_valid <- as.matrix(mtcars[-c(1:20), -1])
fam <- "gaussian"

# Collect data in data.frames
train <- data.frame(y = y_train, X_train)
valid <- data.frame(y = y_valid, X_valid)

# Fit model
mod_lm <- lm(y ~ ., data = train)

# Obtain predictions on new data
pred_lm <- predict(mod_lm, newdata = valid)

# Obtain likelihood
LL_out <- LL_gaussian(
    y = valid$y,
    x = valid[, -1],
    mod = mod_lm
)

# Use the function with different training and validation data
mod_out <- LL_newdata(
    y_train = y_train,
    y_valid = y_valid,
    X_train = X_train,
    X_valid = X_valid,
    fam = "gaussian"
)

# Define tolerance for test
tol <- 1e-5

# Test log-likelihood value is as expected
testthat::expect_true(as.numeric(logLik(mod_out$mod)) != mod_out$LL)
testthat::expect_true(LL_out$ll - mod_out$LL < tol)

# Test: logistic regression ----------------------------------------------------

# Create a copy of the data
mtcars_fact <- mtcars

# Transform dv to factor
mtcars_fact$am <- factor(
    x = mtcars$am,
    levels = c(0, 1),
    labels = c("automatic", "manual")
)

# Use the function with different training and validation data
mod_out <- LL_newdata(
    y_train = mtcars_fact[, "am"],
    y_valid = mtcars_fact[, "am"],
    X_train = as.matrix(mtcars_fact[, 1:3]),
    X_valid = as.matrix(mtcars_fact[, 1:3]),
    fam = "binomial"
)

# Test output object has expected length
testthat::expect_true(length(mod_out) == 3)

# Test: Multinomial logistic regression ----------------------------------------

# Create a copy of the data
mtcars_fact <- mtcars

# Transform dv to factor
mtcars_fact$gear <- factor(
    x = mtcars$gear
)

# Use the function with different training and validation data
mod_out <- LL_newdata(
    y_train = mtcars_fact[, "gear"],
    y_valid = mtcars_fact[, "gear"],
    X_train = as.matrix(mtcars_fact[, 1:3]),
    X_valid = as.matrix(mtcars_fact[, 1:3]),
    fam = "baseline"
)

# Test output object has expected length
testthat::expect_true(length(mod_out) == 3)

# Test: NULL predictor goes to null model --------------------------------------

# Use the function with NULL training and validation data
mod_out <- LL_newdata(
    y_train = mtcars_fact[, "gear"],
    y_valid = mtcars_fact[, "gear"],
    X_train = NULL,
    X_valid = NULL,
    fam = "baseline"
)

testthat::expect_true(ncol(coef(mod_out$mod)) == 1)

# Test: proportional odds model ------------------------------------------------

# Transform dv to factor
mtcars_fact$carb <- factor(
    x = mtcars$carb, ordered = TRUE
)

# Use the function with different training and validation data
mod_out <- LL_newdata(
    y_train = mtcars_fact[, "carb"],
    y_valid = mtcars_fact[, "carb"],
    X_train = as.matrix(mtcars_fact[, 1:3]),
    X_valid = as.matrix(mtcars_fact[, 1:3]),
    fam = "cumulative"
)

# Test output object has expected length
testthat::expect_true(length(mod_out) == 3)
