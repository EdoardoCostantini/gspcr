# Project:   gspcr
# Objective: Test est_univ_mods.R function
# Author:    Edoardo Costantini
# Created:   2023-04-13
# Modified:  2023-04-20
# Notes: 

# Need to evaluate the combo of these data types
dv_types <- names(GSPCRexdata$y)
ivs_types <- names(GSPCRexdata$X)

# Make the combinations
est_univ_mods_conds <- expand.grid(dv_types, ivs_types)

# Look at the combinations and think what should be tested
est_univ_mods_conds

# Test: continuous dv, continuous preds ----------------------------------------

dv_con_ivs_con <- est_univ_mods(
    dv = GSPCRexdata$y$cont,
    ivs = GSPCRexdata$X$cont,
    fam = "gaussian"
)

# Null log-likelihood is returned
testthat::expect_true(dv_con_ivs_con$ll0 < 0)

# Single predictor log-likelihoods are returned
testthat::expect_true(all(dv_con_ivs_con$lls < 0))

# Null log-likelihood < Single predictor log-likelihoods are returned
testthat::expect_true(all(dv_con_ivs_con$ll0 < dv_con_ivs_con$lls))

# Test: binary dv, continuous preds --------------------------------------------

dv_bin_ivs_con <- est_univ_mods(
    dv = GSPCRexdata$y$bin,
    ivs = GSPCRexdata$X$cont,
    fam = "binomial"
)

# Null log-likelihood is returned
testthat::expect_true(dv_bin_ivs_con$ll0 < 0)

# Single predictor log-likelihoods are returned
testthat::expect_true(all(dv_bin_ivs_con$lls < 0))

# Null log-likelihood < Single predictor log-likelihoods are returned
testthat::expect_true(all(dv_bin_ivs_con$ll0 < dv_bin_ivs_con$lls))

# Test: ordinal dv, continuous preds -------------------------------------------

dv_ord_ivs_con <- est_univ_mods(
    dv = GSPCRexdata$y$ord,
    ivs = GSPCRexdata$X$cont,
    fam = "cumulative"
)

# Null log-likelihood is returned
testthat::expect_true(dv_ord_ivs_con$ll0 < 0)

# Single predictor log-likelihoods are returned
testthat::expect_true(all(dv_ord_ivs_con$lls < 0))

# Null log-likelihood < Single predictor log-likelihoods are returned
testthat::expect_true(all(dv_ord_ivs_con$ll0 < dv_ord_ivs_con$lls))

# Test: categorical dv, continuous preds ---------------------------------------

dv_cat_ivs_con <- est_univ_mods(
    dv = GSPCRexdata$y$cat,
    ivs = GSPCRexdata$X$cont,
    fam = "baseline"
)

# Null log-likelihood is returned
testthat::expect_true(dv_cat_ivs_con$ll0 < 0)

# Single predictor log-likelihoods are returned
testthat::expect_true(all(dv_cat_ivs_con$lls < 0))

# Null log-likelihood < Single predictor log-likelihoods are returned
testthat::expect_true(all(dv_cat_ivs_con$ll0 < dv_cat_ivs_con$lls))

# Test: poisson dv, continuous preds -------------------------------------------

dv_poi_ivs_con <- est_univ_mods(
    dv = GSPCRexdata$y$pois,
    ivs = GSPCRexdata$X$cont,
    fam = "poisson"
)

# Null log-likelihood is returned
testthat::expect_true(dv_poi_ivs_con$ll0 < 0)

# Single predictor log-likelihoods are returned
testthat::expect_true(all(dv_poi_ivs_con$lls < 0))

# Null log-likelihood < Single predictor log-likelihoods are returned
testthat::expect_true(all(dv_poi_ivs_con$ll0 < dv_poi_ivs_con$lls))

# Test: continuous dv, mixed preds ---------------------------------------------

dv_con_ivs_mix <- est_univ_mods(
    dv = GSPCRexdata$y$cont,
    ivs = GSPCRexdata$X$mix,
    fam = "gaussian"
)

# Null log-likelihood is returned
testthat::expect_true(dv_con_ivs_mix$ll0 < 0)

# Single predictor log-likelihoods are returned
testthat::expect_true(all(dv_con_ivs_mix$lls < 0))

# Null log-likelihood < Single predictor log-likelihoods are returned
testthat::expect_true(all(dv_con_ivs_mix$ll0 < dv_con_ivs_mix$lls))

# Test: binary dv, mixed preds -------------------------------------------------

dv_bin_ivs_mix <- est_univ_mods(
    dv = GSPCRexdata$y$bin,
    ivs = GSPCRexdata$X$mix,
    fam = "binomial"
)

# Null log-likelihood is returned
testthat::expect_true(dv_bin_ivs_mix$ll0 < 0)

# Single predictor log-likelihoods are returned
testthat::expect_true(all(dv_bin_ivs_mix$lls < 0))

# Null log-likelihood < Single predictor log-likelihoods are returned
testthat::expect_true(all(dv_bin_ivs_mix$ll0 < dv_bin_ivs_mix$lls))

# Test: ordinal dv, mixed preds ------------------------------------------------

dv_ord_ivs_mix <- est_univ_mods(
    dv = GSPCRexdata$y$ord,
    ivs = GSPCRexdata$X$mix,
    fam = "cumulative"
)

# Null log-likelihood is returned
testthat::expect_true(dv_ord_ivs_mix$ll0 < 0)

# Single predictor log-likelihoods are returned
testthat::expect_true(all(dv_ord_ivs_mix$lls < 0))

# Null log-likelihood < Single predictor log-likelihoods are returned
testthat::expect_true(all(dv_ord_ivs_mix$ll0 < dv_ord_ivs_mix$lls))

# Test: categorical dv, mixed preds --------------------------------------------

dv_cat_ivs_mix <- est_univ_mods(
    dv = GSPCRexdata$y$cat,
    ivs = GSPCRexdata$X$mix,
    fam = "baseline"
)

# Null log-likelihood is returned
testthat::expect_true(dv_cat_ivs_mix$ll0 < 0)

# Single predictor log-likelihoods are returned
testthat::expect_true(all(dv_cat_ivs_mix$lls < 0))

# Null log-likelihood < Single predictor log-likelihoods are returned
testthat::expect_true(all(dv_cat_ivs_mix$ll0 < dv_cat_ivs_mix$lls))

# Test: poisson dv, mixed preds ------------------------------------------------

dv_poi_ivs_mix <- est_univ_mods(
    dv = GSPCRexdata$y$pois,
    ivs = GSPCRexdata$X$mix,
    fam = "poisson"
)

# Null log-likelihood is returned
testthat::expect_true(dv_poi_ivs_mix$ll0 < 0)

# Single predictor log-likelihoods are returned
testthat::expect_true(all(dv_poi_ivs_mix$lls < 0))

# Null log-likelihood < Single predictor log-likelihoods are returned
testthat::expect_true(all(dv_poi_ivs_mix$ll0 < dv_poi_ivs_mix$lls))

# Test: categorical dv, categorical preds --------------------------------------

dv_cat_ivs_cat <- est_univ_mods(
    dv = GSPCRexdata$y$cat,
    ivs = GSPCRexdata$X$cat,
    fam = "baseline"
)

# Null log-likelihood is returned
testthat::expect_true(dv_cat_ivs_cat$ll0 < 0)

# Single predictor log-likelihoods are returned
testthat::expect_true(all(dv_cat_ivs_cat$lls < 0))

# Null log-likelihood < Single predictor log-likelihoods are returned
testthat::expect_true(all(dv_cat_ivs_cat$ll0 < dv_cat_ivs_cat$lls))

# Test: ordinal dv, categorical preds --------------------------------------

dv_ord_ivs_cat <- est_univ_mods(
    dv = GSPCRexdata$y$ord,
    ivs = GSPCRexdata$X$cat,
    fam = "cumulative"
)

# Null log-likelihood is returned
testthat::expect_true(dv_ord_ivs_cat$ll0 < 0)

# Single predictor log-likelihoods are returned
testthat::expect_true(all(dv_ord_ivs_cat$lls < 0))

# Null log-likelihood < Single predictor log-likelihoods are returned
testthat::expect_true(all(dv_ord_ivs_cat$ll0 < dv_ord_ivs_cat$lls))

# Test: perfect prediction has expected behavior -------------------------------

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

# Fit all bivariate models
suppressWarnings(
    mod_results <- est_univ_mods(
        dv = y,
        ivs = x,
        fam = "binomial"
    )
)

# Log-likelihood value is returned for the model with perfect predictor
testthat::expect_true(is.numeric(mod_results$lls[1]))

# Coefficient is returned for the the perfect predictor
testthat::expect_true(is.numeric(mod_results$coef[1]))