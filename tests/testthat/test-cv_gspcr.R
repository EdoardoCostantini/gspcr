# Project:   gspcr
# Objective: Test the cv_gspcr function
# Author:    Edoardo Costantini
# Created:   2023-03-16
# Modified:  2023-11-21
# Notes: 

# Define tolerance
tol <- 1e-5

# Test: Continuous outcome -----------------------------------------------------

# Set seed
set.seed(20230727)

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
testthat::expect_equal(length(out_cont), 12)

# Test the class of the output
testthat::expect_equal(class(out_cont), c("gspcrcv", "list"))

# Test: save call works as expected --------------------------------------------

# Set seed
set.seed(20230727)

# Train model to tune parameters
out_no_call <- cv_gspcr(
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
    oneSE = TRUE,
    save_call = FALSE
)

# Test call is NULL
testthat::expect_null(out_no_call$gspcr_call)

# Test result is the same if call is store and not
testthat::expect_equal(out_no_call$sol_table, out_cont$sol_table)

# Test: Binary outcome ---------------------------------------------------------

# Runs without error
testthat::expect_error(
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
    ),
    regexp = NA
)

# Test the class of the output
testthat::expect_equal(class(out_bin), c("gspcrcv", "list"))

# Test: Multi-categorical outcome ----------------------------------------------

# Runs without error
testthat::expect_error(
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
    ),
    regexp = NA
)

# Test the class of the output
testthat::expect_equal(class(out_cat), c("gspcrcv", "list"))

# Test: Ordinal outcome --------------------------------------------------------

# Runs without error
testthat::expect_error(
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
    ),
    regexp = NA
)

# Test the class of the output
testthat::expect_equal(class(out_ord), c("gspcrcv", "list"))

# Test: Count outcome --------------------------------------------------------

# Runs without error
testthat::expect_error(
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
    ),
    regexp = NA
)

# Test the class of the output
testthat::expect_equal(class(out_pois), c("gspcrcv", "list"))

# Test: LLS as threshold -------------------------------------------------------

# Runs without error
testthat::expect_error(
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
    ),
    regexp = NA
)

# Test the class of the output
testthat::expect_equal(class(out_cont_F_lls), c("gspcrcv", "list"))

# Test: PR2 as threshold -------------------------------------------------------

# Runs without error
testthat::expect_error(
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
    ),
    regexp = NA
)

# Test the class of the output
testthat::expect_equal(class(out_cont_F_PR2), c("gspcrcv", "list"))

# Test: Target number of components --------------------------------------------

# Runs without error
testthat::expect_error(
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
    ),
    regexp = NA
)

# Test the length of the output is as expected
testthat::expect_equal(length(out_traget_npcs), 12)

# Test the class of the output
testthat::expect_equal(class(out_traget_npcs), c("gspcrcv", "list"))

# Test: Works with an arbitrary target of number of components -----------------

# Runs without error
testthat::expect_error(
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
    ),
    regexp = NA
)

# Test the length of the output is as expected
testthat::expect_equal(length(out_traget_npcs), 12)

# Test the class of the output
testthat::expect_equal(class(out_traget_npcs), c("gspcrcv", "list"))

# Test: Works with wrong input number of ---------------------------------------

# Excessive npc value
ev <- ncol(GSPCRexdata$X$cont) + 1

# Should return a warning
testthat::expect_warning(
    out_wrong_npcs <- cv_gspcr(
        dv = GSPCRexdata$y$cont,
        ivs = GSPCRexdata$X$cont,
        fam = "gaussian",
        nthrs = 5,
        npcs_range = c(1, 3, 5, ev),
        K = 3,
        fit_measure = "F",
        thrs = "normalized",
        min_features = 1,
        max_features = ncol(GSPCRexdata$X$cont),
        oneSE = TRUE
    )
)

# Test ev is not in the npcs_range used
testthat::expect_false(
    ev %in% out_wrong_npcs$gspcr_call$npcs_range
)

# Test: Works with mixed predictor matrix input --------------------------------

# Runs without error
testthat::expect_error(
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
    ),
    regexp = NA
)

# Test the length of the output is as expected
testthat::expect_equal(length(out_X_mix), 12)

# Test the class of the output
testthat::expect_equal(class(out_X_mix), c("gspcrcv", "list"))

# Test: handle constants in data -----------------------------------------------

# A dataset with a constant variable
X_constant <- iris[1:50, -1]

# Runs without error
testthat::expect_error(
    out_X_constant <- cv_gspcr(
        dv = iris[1:50, 1],
        ivs = X_constant,
        nthrs = 5,
        K = 2,
        min_features = 1,
        max_features = ncol(iris[1:50, -1]),
        npcs_range = 1:2
    ),
    regexp = NA
)

# Test: treatment of ordered factor is as nominal variables --------------------

set.seed(1234)

# Use cv_gspcr on ordinal predictors
out_X_ord <- cv_gspcr(
    dv = GSPCRexdata$y$cont,
    ivs = GSPCRexdata$X$ord,
    nthrs = 5,
    npcs_range = 1:2,
    K = 3
)

set.seed(1234)

# Use pca_mix on nominal data
out_X_cat <- cv_gspcr(
    dv = GSPCRexdata$y$cont,
    ivs = GSPCRexdata$X$cat,
    nthrs = 5,
    npcs_range = 1:2,
    K = 3
)

# Check is the result an error?
testthat::expect_equal(out_X_ord$sol_table, out_X_cat$sol_table, tolerance = tol)

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

# There are warnings without perfect predictor
testthat::expect_warning(
    gscpr_fit <- gspcr::cv_gspcr(
        dv = y,
        ivs = x[, -1],
        fam = "binomial",
        fit_measure = "BIC",
        thrs = "PR2"
    ),
    regexp = NA
)

# There are warnings with perfect predictor
suppressWarnings(
    testthat::expect_warning(
        gscpr_fit <- gspcr::cv_gspcr(
            dv = y,
            ivs = x,
            fam = "binomial",
            fit_measure = "BIC",
            thrs = "PR2"
        )
    )
)