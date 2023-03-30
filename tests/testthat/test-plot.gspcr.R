# Project:   gspcr
# Objective: Testing the plot.gspcrout function
# Author:    Edoardo Costantini
# Created:   2023-03-29
# Modified:  2023-03-30
# Notes: 

# Does the function produce a ggplot output? -----------------------------------

# Train the GSPCR model
out1 <- cv_gspcr(
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
plot_output <- plot(
    x = out1,
    y = "LSS",
    labels = TRUE, 
    errorBars = FALSE, 
    discretize = TRUE,
    print = FALSE # not needed for test
)

# Perform the test
testthat::expect_true(ggplot2::is.ggplot(plot_output))

# Can you change plotting parameters? ------------------------------------------

# Change the shape of the points
# plot_output <- plot(
#     x = out1,
#     labels = FALSE,
#     errorBars = FALSE,
#     discretize = TRUE,
#     shape = 14,
#     print = FALSE # not needed for test
# )