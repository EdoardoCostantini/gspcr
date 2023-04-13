# Project:   gspcr
# Objective: Testing the plot.gspcrout function
# Author:    Edoardo Costantini
# Created:   2023-03-29
# Modified:  2023-04-13
# Notes: 

# Test: ggplot as output -------------------------------------------------------

# Train the GSPCR model
out <- cv_gspcr(
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

# Use the plotting function
plot_output <- plot(
    x = out,
    y = "F",
    labels = TRUE, 
    errorBars = FALSE, 
    discretize = TRUE,
    print = FALSE # not needed for fit_measure
)

# Perform the test
testthat::expect_true(ggplot2::is.ggplot(plot_output))

# Test: plotting parameters can be changed -------------------------------------

# # Change the shape of the points
# plot_output <- plot(
#     x = out,
#     labels = FALSE,
#     errorBars = FALSE,
#     discretize = TRUE,
#     shape = 14,
#     print = FALSE # not needed for test
# )