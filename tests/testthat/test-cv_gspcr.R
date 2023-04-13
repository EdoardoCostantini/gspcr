# Project:   gspcr
# Objective: Test the cv_gspcr function
# Author:    Edoardo Costantini
# Created:   2023-03-16
# Modified:  2023-04-12
# Notes: 

# Use the functions with a given method
out1 <- cv_gspcr(
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
testthat::expect_equal(length(out1), 10)

# Test the class of the output
testthat::expect_equal(class(out1), c("gspcrout", "list"))

# Binary outcome variable ------------------------------------------------------

# Use the functions with a given method
out1 <- cv_gspcr(
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
testthat::expect_equal(class(out1), c("gspcrout", "list"))

# Multi-categorical outcome variable -------------------------------------------

# Use the functions with a given method
out1 <- cv_gspcr(
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
testthat::expect_equal(class(out1), c("gspcrout", "list"))

# Ordinal outcome variable -----------------------------------------------------

# Use the functions with a given method
out1 <- cv_gspcr(
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
testthat::expect_equal(class(out1), c("gspcrout", "list"))