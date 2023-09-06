# Project:   gspcr
# Objective: Test predict.gspcrout
# Author:    Edoardo Costantini
# Created:   2023-06-01
# Modified:  2023-07-27
# Notes: 

# Set a seed
set.seed(20230724)

# Separate training data from the rest
train <- sample(
    x = 1:nrow(GSPCRexdata$X$cont),
    size = nrow(GSPCRexdata$X$cont) * 99/100
)

# Continuous outcome -----------------------------------------------------------

# CV GSPCR on training data
out_cont <- cv_gspcr(
    dv = GSPCRexdata$y$cont[train],
    ivs = GSPCRexdata$X$cont[train, ],
    fam = "gaussian",
    thrs = "normalized",
    nthrs = 5,
    npcs_range = 1:5,
    K = 3,
    fit_measure = "F",
    min_features = 1,
    max_features = ncol(GSPCRexdata$X$cont),
    oneSE = TRUE
)

# Estimate GSPCR
gspcr_est <- est_gspcr(
    dv = GSPCRexdata$y$cont[train],
    ivs = GSPCRexdata$X$cont[train, ],
    fam = "gaussian",
    ndim = out_cont$sol_table[1, "Q"],
    active_set = out_cont$pred_map[, out_cont$sol_table[1, "thr_number"]]
)

# Predict on same data
y_hat_in_sample <- predict(
    object = gspcr_est
)

# Predict on same data manually
y_hat_in_sample_man <- predict(
    object = gspcr_est,
    newdata = GSPCRexdata$X$cont[train, ]
)

# Predict new data
y_hat_out_sample <- predict(
    object = gspcr_est,
    newdata = GSPCRexdata$X$cont[-train, ]
)

# Test prediction on same data different modes
testthat::expect_equal(y_hat_in_sample, y_hat_in_sample_man)

# Test values are numeric
testthat::expect_true(all(is.numeric(y_hat_in_sample)))
testthat::expect_true(all(is.numeric(y_hat_out_sample)))

# Binary outcome ---------------------------------------------------------------

# CV GSPCR on training data
out_bin <- cv_gspcr(
    dv = GSPCRexdata$y$bin[train],
    ivs = GSPCRexdata$X$cont[train, ],
    fam = "binomial",
    nthrs = 5,
    npcs_range = 1:5,
    K = 3,
    fit_measure = "LRT",
    thrs = "PR2",
    min_features = 1,
    max_features = ncol(GSPCRexdata$X$cont),
    oneSE = TRUE
)

# Estimate GSPCR
gspcr_est <- est_gspcr(
    dv = GSPCRexdata$y$bin[train],
    ivs = GSPCRexdata$X$cont[train, ],
    fam = "binomial",
    ndim = out_bin$sol_table[1, "Q"],
    active_set = out_bin$pred_map[, out_bin$sol_table[1, "thr_number"]]
)

# Predict on same data
y_hat <- predict(
    object = gspcr_est
)

# Predict new data
y_hat <- predict(
    object = gspcr_est,
    newdata = GSPCRexdata$X$cont[-train, ]
)

# Test values are between 0 and 1
testthat::expect_true(all(0 < y_hat & y_hat < 1))

# Ordinal outcome ---------------------------------------------------------------

# CV GSPCR on training data
out_ord <- cv_gspcr(
    dv = GSPCRexdata$y$ord[train],
    ivs = GSPCRexdata$X$cont[train, ],
    fam = "cumulative",
    nthrs = 5,
    npcs_range = 1:5,
    K = 3,
    fit_measure = "LRT",
    thrs = "PR2",
    min_features = 1,
    max_features = ncol(GSPCRexdata$X$cont),
    oneSE = TRUE
)

# Estimate GSPCR
gspcr_est <- est_gspcr(
    dv = GSPCRexdata$y$ord[train],
    ivs = GSPCRexdata$X$cont[train, ],
    fam = "cumulative",
    ndim = out_ord$sol_table[1, "Q"],
    active_set = out_ord$pred_map[, out_ord$sol_table[1, "thr_number"]]
)

# Predict on same data
y_hat <- predict(
    object = gspcr_est
)

# Predict new data
y_hat <- predict(
    object = gspcr_est,
    newdata = GSPCRexdata$X$cont[-train, ]
)

# Test values are between 0 and 1
testthat::expect_true(all(0 < y_hat & y_hat < 1))

# Test the number of categories is right
testthat::expect_true(ncol(y_hat) == nlevels(GSPCRexdata$y$ord))

# Multinomial outcome ---------------------------------------------------------------

# Use the functions with a given method
out_cat <- cv_gspcr(
    dv = GSPCRexdata$y$cat[train],
    ivs = GSPCRexdata$X$cont[train, ],
    fam = "baseline",
    nthrs = 5,
    npcs_range = 1:5,
    K = 3,
    fit_measure = "LRT",
    thrs = "PR2",
    min_features = 1,
    max_features = ncol(GSPCRexdata$X$cont),
    oneSE = TRUE
)

# Estimate GSPCR
gspcr_est <- est_gspcr(
    dv = GSPCRexdata$y$cat[train],
    ivs = GSPCRexdata$X$cont[train, ],
    fam = "baseline",
    ndim = out_cat$sol_table[1, "Q"],
    active_set = out_cat$pred_map[, out_cat$sol_table[1, "thr_number"]]
)

# Predict on same data
y_hat <- predict(
    object = gspcr_est
)

# Predict new data
y_hat <- predict(
    object = gspcr_est,
    newdata = GSPCRexdata$X$cont[-train, ]
)

# Test all values are between 0 and 1
testthat::expect_true(all(0 < y_hat & y_hat < 1))

# Test the number of categories is right
testthat::expect_true(ncol(y_hat) == nlevels(GSPCRexdata$y$cat))

# Predict new data with a constant categorical variable ------------------------
# Note: See predict.gspcrout for details on why this test is needed.

# Make a new dataset with one categorical variable having a single value
test_contants <- GSPCRexdata$X$mix[-train, ][1:4, ]

# Make a new dataset with no categorical variable having a single value
test_no_contast <- GSPCRexdata$X$mix[-train, ]

# Use the functions with a given method
out_cat <- cv_gspcr(
    dv = GSPCRexdata$y$cont[train],
    ivs = GSPCRexdata$X$mix[train, ],
    fam = "gaussian",
    nthrs = 5,
    npcs_range = 1:5,
    K = 3,
    fit_measure = "LRT",
    thrs = "PR2",
    min_features = 1,
    max_features = ncol(GSPCRexdata$X$cont),
    oneSE = TRUE
)

# Estimate GSPCR
gspcr_est <- est_gspcr(
    dv = GSPCRexdata$y$cont[train],
    ivs = GSPCRexdata$X$mix[train, ],
    fam = "gaussian",
    ndim = out_cat$sol_table[1, "Q"],
    active_set = out_cat$pred_map[, out_cat$sol_table[1, "thr_number"]]
)

# Predict new data with constants
y_hat_constants <- predict(
    object = gspcr_est,
    newdata = test_contants
)

# Predict new data without constants
y_hat_no_constants <- predict(
    object = gspcr_est,
    newdata = test_no_contast
)

# Test the same values are predicted for the same observations
testthat::expect_true(
    object = all.equal(
        y_hat_constants[names(y_hat_constants)],
        y_hat_no_constants[names(y_hat_constants)]
    )
)

# Test: predict when constant in present in data -------------------------------

# A dataset with a constant variable
X_constant <- iris[1:50, -1]

# Use the functions with a given method
out_X_constant <- cv_gspcr(
    dv = iris[1:50, 1],
    ivs = X_constant,
    nthrs = 5,
    K = 2,
    min_features = 1,
    max_features = ncol(iris[1:50, -1]),
    npcs_range = 1:2
)

# Estimate GSPCR
gspcr_est <- est_gspcr(
    dv = iris[1:50, 1],
    ivs = X_constant,
    fam = "gaussian",
    ndim = out_X_constant$sol_table[1, "Q"],
    active_set = names(out_X_constant$pred_map[, out_X_constant$sol_table[1, "thr_number"]])
)

# A new dataset with a constant variable
X_constant_new <- iris[sample(1:50, 10, replace = TRUE), -1]

# Predict old data
y_hat_old <- predict(
    object = gspcr_est
)

# Predict new data
y_hat_new <- predict(
    object = gspcr_est,
    newdata = X_constant_new
)

# Test values are numeric
testthat::expect_true(all(is.numeric(y_hat_old)))
testthat::expect_true(all(is.numeric(y_hat_new)))