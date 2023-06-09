# Project:   gspcr
# Objective: Test the cv_gspcr function
# Author:    Edoardo Costantini
# Created:   2023-03-16
# Modified:  2023-04-13
# Notes: 

# Test: Continuous outcome -----------------------------------------------------

# Use the functions with a given method
out_cont <- cv_gspcr(
    dv = GSPCRexdata$y$cont,
    ivs = GSPCRexdata$X,
    fam = "gaussian",
    nthrs = 5,
    maxnpcs = 5,
    K = 3,
    fit_measure = "F",
    thrs = "normalized",
    min_features = 1,
    max_features = ncol(GSPCRexdata$X),
    oneSE = TRUE
)

# Test the length of the output is as expected
testthat::expect_equal(length(out_cont), 10)

# Test the class of the output
testthat::expect_equal(class(out_cont), c("gspcrout", "list"))

# Test: Binary outcome ---------------------------------------------------------

# Use the functions with a given method
out_bin <- cv_gspcr(
    dv = GSPCRexdata$y$bin,
    ivs = GSPCRexdata$X,
    fam = "binomial",
    nthrs = 5,
    maxnpcs = 5,
    K = 3,
    fit_measure = "LRT",
    thrs = "PR2",
    min_features = 1,
    max_features = ncol(GSPCRexdata$X),
    oneSE = TRUE
)

# Test the class of the output
testthat::expect_equal(class(out_bin), c("gspcrout", "list"))

# Test: Multi-categorical outcome ----------------------------------------------

# Use the functions with a given method
out_cat <- cv_gspcr(
    dv = GSPCRexdata$y$cat,
    ivs = GSPCRexdata$X,
    fam = "baseline",
    nthrs = 5,
    maxnpcs = 5,
    K = 3,
    fit_measure = "LRT",
    thrs = "PR2",
    min_features = 1,
    max_features = ncol(GSPCRexdata$X),
    oneSE = TRUE
)

# Test the class of the output
testthat::expect_equal(class(out_cat), c("gspcrout", "list"))

# Test: Ordinal outcome --------------------------------------------------------

# Use the functions with a given method
out_ord <- cv_gspcr(
    dv = GSPCRexdata$y$ord,
    ivs = GSPCRexdata$X,
    fam = "cumulative",
    nthrs = 5,
    maxnpcs = 5,
    K = 3,
    fit_measure = "LRT",
    thrs = "PR2",
    min_features = 1,
    max_features = ncol(GSPCRexdata$X),
    oneSE = TRUE
)

# Test the class of the output
testthat::expect_equal(class(out_ord), c("gspcrout", "list"))

# Test: Count outcome --------------------------------------------------------

# Use the functions with a given method
out_pois <- cv_gspcr(
    dv = GSPCRexdata$y$pois,
    ivs = GSPCRexdata$X,
    fam = "poisson",
    nthrs = 5,
    maxnpcs = 5,
    K = 3,
    fit_measure = "LRT",
    thrs = "PR2",
    min_features = 1,
    max_features = ncol(GSPCRexdata$X),
    oneSE = TRUE
)

# Test the class of the output
testthat::expect_equal(class(out_pois), c("gspcrout", "list"))