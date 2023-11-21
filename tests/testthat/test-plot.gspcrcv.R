# Project:   gspcr
# Objective: Testing the plot.gspcrcv function
# Author:    Edoardo Costantini
# Created:   2023-03-29
# Modified:  2023-11-21
# Notes: 

# Test: ggplot as output -------------------------------------------------------

# Train the GSPCR model
out <- cv_gspcr(
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

# Test: warning if wrong y is requested ----------------------------------------

testthat::expect_warning(
    plot(
        x = out,
        y = "PR2",
        labels = FALSE,
        errorBars = FALSE,
        discretize = TRUE,
        print = FALSE # not needed for test
    )
)

# Test: pseudo-R2 is used for both thresholding and fit measure ----------------

# Use the function with pseudo-R2 as both thresholding and fit measure
testthat::expect_error(
    cv_gspcr(
        dv = GSPCRexdata$y$cat,
        ivs = GSPCRexdata$X$cont,
        fam = "baseline",
        nthrs = 5,
        npcs_range = 1:5,
        K = 3,
        fit_measure = "PR2",
        thrs = "PR2",
        min_features = 1,
        max_features = ncol(GSPCRexdata$X$cont),
        oneSE = TRUE
    ),
    regexp = NA
)