# Project:   gspcr
# Objective: Test the cv.gspcr function
# Author:    Edoardo Costantini
# Created:   2023-03-16
# Modified:  2023-03-16
# Notes: 

# Use the functions with a given method
out1 <- cv.gspcr(
    dv = GSPCRexdata$y,
    ivs = GSPCRexdata[, -1],
    fam = "gaussian",
    nthrs = 5,
    maxnpcs = 5,
    K = 3,
    test = "F",
    thrs = "normalized",
    min.features = 1,
    max.features = ncol(GSPCRexdata[, -1])
)

# Test the length of the output is as expected
testthat::expect_equal(length(out1), 9)
