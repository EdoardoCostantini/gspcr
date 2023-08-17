# Project:   gspcr
# Objective: Test estimation function
# Author:    Edoardo Costantini
# Created:   2023-07-27
# Modified:  2023-08-15
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
    active_set = out$solution$standard$active_set, 
    ndim = out$solution$standard$Q
)

# Estimate GSPCR with direct input based on object
gspcr_est_direct_solution <- est_gspcr(
    dv = iris[, 1],
    ivs = iris[, -1],
    fam = "gaussian",
    active_set = out$solution$standard$active_set,
    ndim = out$solution$standard$Q
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
testthat::expect_equal(
    gspcr_est_out$glm_fit$deviance,
    gspcr_est_direct_solution$glm_fit$deviance
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