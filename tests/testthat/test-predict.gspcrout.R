# Project:   gspcr
# Objective: #TODO
# Author:    Edoardo Costantini
# Created:   2023-06-01
# Modified:  2023-06-01
# Notes: 

# Separate training data from the rest
train <- sample(
    x = 1:nrow(GSPCRexdata$X$cont),
    size = nrow(GSPCRexdata$X$cont) * 3 / 4
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
    ndim = max(2, out_cont$sol_table[1, "Q"]),
    active_set = out_cont$pred_map[, out_cont$sol_table[1, "thr_number"]]
)

# Predict new Y data
y_hat <- predict.gspcrout(
    glm_fit = gspcr_est$glm_fit,
    pcamix = gspcr_est$pca_mix_out$pcamix, 
    x = GSPCRexdata$X$cont[-train, out_cont$pred_map[, out_cont$sol_table[1, "thr_number"]]]
)

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
    ndim = max(2, out_bin$sol_table[1, "Q"]),
    active_set = out_bin$pred_map[, out_bin$sol_table[1, "thr_number"]]
)

# Predict new Y data
y_hat <- predict.gspcrout(
    glm_fit = gspcr_est$glm_fit,
    pcamix = gspcr_est$pca_mix_out$pcamix, 
    fam = "binomial",
    x = GSPCRexdata$X$cont[-train, out_bin$pred_map[, out_bin$sol_table[1, "thr_number"]]]
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
    ndim = max(2, out_ord$sol_table[1, "Q"]),
    active_set = out_ord$pred_map[, out_ord$sol_table[1, "thr_number"]]
)

# Predict new Y data
y_hat <- predict.gspcrout(
    glm_fit = gspcr_est$glm_fit,
    pcamix = gspcr_est$pca_mix_out$pcamix, 
    fam = "cumulative",
    x = GSPCRexdata$X$cont[-train, out_ord$pred_map[, out_ord$sol_table[1, "thr_number"]]]
)

# Test values are between 0 and 1
testthat::expect_true(all(0 < y_hat & y_hat < 1))

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
    ndim = max(2, out_cat$sol_table[1, "Q"]),
    active_set = out_cat$pred_map[, out_cat$sol_table[1, "thr_number"]]
)

# Predict new Y data
y_hat <- predict.gspcrout(
    glm_fit = gspcr_est$glm_fit,
    pcamix = gspcr_est$pca_mix_out$pcamix, 
    fam = "baseline",
    x = GSPCRexdata$X$cont[-train, out_cat$pred_map[, out_cat$sol_table[1, "thr_number"]]]
)

# Test all values are between 0 and 1
testthat::expect_true(all(0 < y_hat & y_hat < 1))
