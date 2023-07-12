# Project:   gspcr
# Objective: Test cp_F function
# Author:    Edoardo Costantini
# Created:   2023-04-18
# Modified:  2023-07-12
# Notes: 

# Define tolerance for differences
tol <- 1e-5

# Test: output class -----------------------------------------------------------

# Fit a null model
lm_n <- lm(mpg ~ 1, data = mtcars)

# Fit a full model
lm_f <- lm(mpg ~ cyl + disp, data = mtcars)

# Compute F statistic with your function
f_M <- cp_F(
    y = mtcars$mpg,
    y_hat_restricted = predict(lm_n),
    y_hat_full = predict(lm_f),
    p_full = 2
)

# Atomic numeric vector
testthat::expect_true(is.numeric(f_M))

# Length 1
testthat::expect_true(length(f_M) == 1)

# Test: manual computation = lm output -----------------------------------------

# Extract the F statistic
f_R <- summary(lm_f)$fstatistic["value"]

# R equal to manual
testthat::expect_true(f_R - f_M < tol)

# Test: Works for incremental models -------------------------------------------

# Fit a more complex full model
lm_f_2 <- lm(mpg ~ cyl + disp + hp + drat + qsec, data = mtcars)

# Perform change in R2 test
change_R2_test <- anova(lm_f_2, lm_f)

# Extract R F statistic
f_change_R <- change_R2_test$F[2]

# Compute F statistic with your function
f_change_M <- cp_F(
    y = mtcars$mpg,
    y_hat_restricted = predict(lm_f),
    y_hat_full = predict(lm_f_2),
    p_restricted = 2,
    p_full = 5
)

# R equal to manual
testthat::expect_true(f_change_R - f_change_M < tol)
