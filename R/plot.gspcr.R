#' Plot the cross-validation solution path for the GSPCR algorithm
#'
#' Produces a scatter plot showing the CV score obtained by \code{cv.gspcr} (Y-axis) with different threshold values (X-axis) for a different number of components (lines).
#'
#' @param x An object of class \code{gspcr}.
#' @param y The CV test to report on the Y axis. Default is the test specified in \code{cv.gspcr()}.
#' @param labels Logical value. \code{FALSE} hides the labels of the points indicating the number of components used. The default is \code{TRUE}.
#' @param errorBars Logical value. \code{TRUE} shows the error bars for each point. The default is \code{FALSE}.
#' @param discretize Logical value. \code{TRUE} treats the X-axis as a discrete measure that facilitates comparing solution paths between different test types. The default is \code{TRUE}.
#' @param print Logical value. TRUE prints the plot when the function is called. The default is \code{TRUE}.
#' @param ... Other arguments passed on to methods. Not currently used.
#' @details
#' The bounds defining the error bars are computed by \code{cv.gspcr()}. First, the K-fold cross-validation score of the statistic of interest (e.g., the F score, the MSE) is computed. Then, the standard deviation of the statistic across the K folds is computed. Finally, the bounds used for the error bars are computed by summing and subtracting this standard deviation to and from the K-fold cross-validation score of the statistic.
#' @return A scatter plot of \code{ggplot} class
#' @author Edoardo Costantini, 2023
#'
#' @export
plot.gspcr <- function(x, y = NULL, labels = TRUE, errorBars = FALSE, discretize = TRUE, print = TRUE, ...) {

    # Check y
    if (is.null(y)) {
        y <- x$gspcr.call$test
    } else {
        # Define the message
        message_wrongy <- paste0(y, " was not used as the score in the CV procedure. If you want to plot the ", y, " scores, run cv.gspcr again and change the `test` argument to ", y, ".")
        # Return it
        warning(message_wrongy)
        # And use the test called by spcr
        y <- x$gspcr.call$test
    }

    # Make scores from wide to long
    x.long <- reshape2::melt(
        data = x$scor,
        value.name = x$gspcr.call$test,
        varnames = c("npcs", x$gspcr.call$thrs)
    )

    # Make npcs a factor
    x.long$npcs <- factor(x.long$npcs)

    # Make the X-axis into factor if required
    if(discretize == TRUE){
        x.long[, x$gspcr.call$thrs] <- factor(
            x = round(x.long[, x$gspcr.call$thrs], 3),
            levels = round(x$thr, 3)
        )
    }

    # Add error bounds
    x.long$low <- reshape2::melt(data = x$scor.lwr)[, "value"]
    x.long$high <- reshape2::melt(x$scor.upr)[, "value"]

    # Make baseline plot
    store_plot <- x.long %>%
        ggplot2::ggplot(
            ggplot2::aes(
                x = .data[[x$gspcr.call$thrs]],
                y = .data[[x$gspcr.call$test]],
                group = .data$npcs,
                label = .data$npcs
            )
        ) + 
        ggplot2::geom_point() + 
        ggplot2::geom_line() +
        ggplot2::theme_bw()
    
    # Avoid dropping empty levels if the X-axis is discrete
    if (discretize == TRUE) {
        store_plot <- store_plot +
            ggplot2::scale_x_discrete(drop = FALSE)
    }

    # Add labels if requested
    if (labels == TRUE) {
        store_plot <- store_plot +
            ggplot2::geom_label()
    }

    # Add error bars if required
    if (errorBars == TRUE) {
        store_plot <- store_plot +
            ggplot2::geom_errorbar(
                ggplot2::aes(
                    ymin = .data$low,
                    ymax = .data$high
                ),
                width = .2
            )
    }

    # Return plot
    store_plot
}