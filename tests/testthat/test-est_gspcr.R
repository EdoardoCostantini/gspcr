# Project:   gspcr
# Objective: Test estimation function
# Author:    Edoardo Costantini
# Created:   2023-07-27
# Modified:  2023-07-27
# Notes: 

# Set a seed
set.seed(20230724)

# Test: works with object input ------------------------------------------------

# CV GSPCR on training data
out <- cv_gspcr(
    dv = iris[, 1],
    ivs = iris[, -1],
    fam = "gaussian",
    thrs = "PR2",
    nthrs = 5,
    npcs_range = 1:3,
    K = 3,
    fit_measure = "F",
    min_features = 1,
    max_features = ncol(iris[, -1]),
    oneSE = TRUE
)

# Estimate GSPCR based on object
gspcr_est_out <- est_gspcr(
    object = out
)

# Estimate GSPCR with direct input based on object
gspcr_est_direct <- est_gspcr(
    dv = out$gspcr_call$dv,
    ivs = out$gspcr_call$ivs, 
    fam = out$gspcr_call$fam, 
    active_set = names(out$pred_map[, out$sol_table[1, "thr_number"]]), 
    ndim = out$sol_table[1, "Q"]
)

# Test methods returned the same fits
testthat::expect_equal(
    gspcr_est_out$glm_fit$coefficients,
    gspcr_est_direct$glm_fit$coefficients
)
testthat::expect_equal(
    gspcr_est_out$glm_fit$deviance,
    gspcr_est_direct$glm_fit$deviance
)

# Test: works with custom direct input -----------------------------------------

# Define some argument decisions
ndim <- 2
active_set <- paste0("X", 3:10)

# Estimate GSPCR
gspcr_est <- est_gspcr(
    dv = GSPCRexdata$y$cont,
    ivs = GSPCRexdata$X$cont,
    fam = "gaussian",
    ndim = ndim,
    active_set = active_set
)

# Test the class of the output
testthat::expect_equal(class(gspcr_est), c("gspcrout", "list"))

# Test used active set is correct
testthat::expect_equal(gspcr_est$active_set, active_set)

# Test: perfect prediction -----------------------------------------------------

# Set seed
set.seed(20230801)

# Generate some predictors
n <- 1e2
p <- 50
Sigma <- matrix(.7, nrow = p, ncol = p)
diag(Sigma) <- 1
x <- data.frame(MASS::mvrnorm(n, rep(0, p), Sigma))

# Create a dv with a perfect predictor
y <- factor(x[, 1] < 0, labels = c("y", "n"))

# Train model to tune parameters
gscpr_fit <- gspcr::cv_gspcr(
    dv = y,
    ivs = x,
    fam = "binomial",
    fit_measure = "BIC",
    thrs = "PR2"
)

# Output is as expected
testthat::expect_equal(class(gscpr_fit), c("gspcrcv", "list"))