# Project:   gspcr
# Objective: Test the cv_gspcr function
# Author:    Edoardo Costantini
# Created:   2023-03-16
# Modified:  2023-07-27
# Notes: 

# Test: Continuous outcome -----------------------------------------------------

# Use the functions with a given method
out_cont <- cv_gspcr(
    dv = GSPCRexdata$y$cont,
    ivs = GSPCRexdata$X$cont,
    fam = "gaussian",
    nthrs = 5,
    npcs_range = 1:5,
    K = 3,
    fit_measure = "F",
    thrs = "normalized",
    min_features = 1,
    max_features = ncol(GSPCRexdata$X$cont),
    oneSE = TRUE
)

# Test the length of the output is as expected
testthat::expect_equal(length(out_cont), 11)

# Test the class of the output
testthat::expect_equal(class(out_cont), c("gspcrcv", "list"))

# Test: Binary outcome ---------------------------------------------------------

# Use the functions with a given method
out_bin <- cv_gspcr(
    dv = GSPCRexdata$y$bin,
    ivs = GSPCRexdata$X$cont,
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

# Test the class of the output
testthat::expect_equal(class(out_bin), c("gspcrcv", "list"))

# Test: Multi-categorical outcome ----------------------------------------------

# Use the functions with a given method
out_cat <- cv_gspcr(
    dv = GSPCRexdata$y$cat,
    ivs = GSPCRexdata$X$cont,
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

# Test the class of the output
testthat::expect_equal(class(out_cat), c("gspcrcv", "list"))

# Test: Ordinal outcome --------------------------------------------------------

# Use the functions with a given method
out_ord <- cv_gspcr(
    dv = GSPCRexdata$y$ord,
    ivs = GSPCRexdata$X$cont,
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

# Test the class of the output
testthat::expect_equal(class(out_ord), c("gspcrcv", "list"))

# Test: Count outcome --------------------------------------------------------

# Use the functions with a given method
out_pois <- cv_gspcr(
    dv = GSPCRexdata$y$pois,
    ivs = GSPCRexdata$X$cont,
    fam = "poisson",
    nthrs = 5,
    npcs_range = 1:5,
    K = 3,
    fit_measure = "LRT",
    thrs = "PR2",
    min_features = 1,
    max_features = ncol(GSPCRexdata$X$cont),
    oneSE = TRUE
)

# Test the class of the output
testthat::expect_equal(class(out_pois), c("gspcrcv", "list"))

# Test: LLS as threshold -------------------------------------------------------

# Use the functions with a given method
out_cont_F_lls <- cv_gspcr(
    dv = GSPCRexdata$y$cont,
    ivs = GSPCRexdata$X$cont,
    fam = "gaussian",
    nthrs = 5,
    npcs_range = 1:5,
    K = 3,
    fit_measure = "F",
    thrs = "LLS",
    min_features = 1,
    max_features = ncol(GSPCRexdata$X$cont),
    oneSE = TRUE
)

# Test the class of the output
testthat::expect_equal(class(out_cont_F_lls), c("gspcrcv", "list"))

# Test: PR2 as threshold -------------------------------------------------------

# Use the functions with a given method
out_cont_F_PR2 <- cv_gspcr(
    dv = GSPCRexdata$y$cont,
    ivs = GSPCRexdata$X$cont,
    fam = "gaussian",
    nthrs = 5,
    npcs_range = 1:5,
    K = 3,
    fit_measure = "F",
    thrs = "PR2",
    min_features = 1,
    max_features = ncol(GSPCRexdata$X$cont),
    oneSE = TRUE
)

# Test the class of the output
testthat::expect_equal(class(out_cont_F_PR2), c("gspcrcv", "list"))

# Test: Target number of components --------------------------------------------

# Use the functions with a given method
out_traget_npcs <- cv_gspcr(
    dv = GSPCRexdata$y$cont,
    ivs = GSPCRexdata$X$cont,
    fam = "gaussian",
    nthrs = 5,
    npcs_range = 1,
    K = 3,
    fit_measure = "F",
    thrs = "normalized",
    min_features = 1,
    max_features = ncol(GSPCRexdata$X$cont),
    oneSE = TRUE
)

# Test the length of the output is as expected
testthat::expect_equal(length(out_traget_npcs), 11)

# Test the class of the output
testthat::expect_equal(class(out_traget_npcs), c("gspcrcv", "list"))

# Test: Works with an arbitrary target of number of components -----------------

# Use the functions with a given method
out_traget_npcs <- cv_gspcr(
    dv = GSPCRexdata$y$cont,
    ivs = GSPCRexdata$X$cont,
    fam = "gaussian",
    nthrs = 5,
    npcs_range = c(1, 3, 5, 7),
    K = 3,
    fit_measure = "F",
    thrs = "normalized",
    min_features = 1,
    max_features = ncol(GSPCRexdata$X$cont),
    oneSE = TRUE
)

# Test the length of the output is as expected
testthat::expect_equal(length(out_traget_npcs), 11)

# Test the class of the output
testthat::expect_equal(class(out_traget_npcs), c("gspcrcv", "list"))

# Test: Works with mixed predictor matrix input --------------------------------

# Use the functions with a given method
out_X_mix <- cv_gspcr(
    dv = GSPCRexdata$y$cont,
    ivs = GSPCRexdata$X$mix,
    fam = "gaussian",
    nthrs = 5,
    npcs_range = 1:3,
    K = 3,
    fit_measure = "F",
    thrs = "PR2",
    min_features = 1,
    max_features = ncol(GSPCRexdata$X$mix),
    oneSE = TRUE
)

# Test the length of the output is as expected
testthat::expect_equal(length(out_X_mix), 11)

# Test the class of the output
testthat::expect_equal(class(out_X_mix), c("gspcrcv", "list"))

# Test: handle constants in data -----------------------------------------------

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

# Test the class of the output
testthat::expect_equal(class(out_X_constant), c("gspcrcv", "list"))