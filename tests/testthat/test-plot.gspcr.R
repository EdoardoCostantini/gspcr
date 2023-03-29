# Project:   gspcr
# Objective: Testing the plot.gspcr function
# Author:    Edoardo Costantini
# Created:   2023-03-29
# Modified:  2023-03-29
# Notes: 

# Does the function produce a ggplot output? -----------------------------------

# Train the GSPCR model
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

# Use the plotting function
plot_output <- plot.gspcr(
    gspcr.out = out1,
    labels = TRUE, 
    errorBars = FALSE, 
    discretize = TRUE,
    print = FALSE # not needed for test
)

# Perform the test
testthat::expect_true(ggplot2::is.ggplot(plot_output))