#' Plot the cross-validation solution path for the GSPCR algorithm
#'
#' Produces a scatter plot showing the CV score obtained by \code{cv_gspcr} (Y-axis) with different threshold values (X-axis) for a different number of components (lines).
#'
#' @param x An object of class \code{gspcr}.
#' @param y The CV fit measure to report on the Y axis. Default is the fit measure specified in \code{cv_gspcr()}.
#' @param labels Logical value. \code{FALSE} hides the labels of the points indicating the number of components used. The default is \code{TRUE}.
#' @param errorBars Logical value. \code{TRUE} shows the error bars for each point. The default is \code{FALSE}.
#' @param discretize Logical value. \code{TRUE} treats the X-axis as a discrete measure that facilitates comparing solution paths between different fit measures. The default is \code{TRUE}.
#' @param y_reverse Logical value. \code{TRUE} reverses the y axis scale. The default is \code{FALSE}.
#' @param print Logical value. TRUE prints the plot when the function is called. The default is \code{TRUE}.
#' @param ... Other arguments passed on to methods. Not currently used.
#' @details
#' The bounds defining the error bars are computed by \code{cv_gspcr()}. First, the K-fold cross-validation score of the statistic of interest (e.g., the F score, the MSE) is computed. Then, the standard deviation of the statistic across the K folds is computed. Finally, the bounds used for the error bars are computed by summing and subtracting this standard deviation to and from the K-fold cross-validation score of the statistic.
#'
#' Reversing the y-axis with \code{y_reverse} can be helpful to compare results obtained by different fit measures.
#' @return A scatter plot of \code{ggplot} class
#' @author Edoardo Costantini, 2023
#'
#' @export
plot.gspcrcv <- function(x, y = NULL, labels = TRUE, errorBars = FALSE, discretize = TRUE, y_reverse = FALSE, print = TRUE, ...) {
    # Check y
    if (is.null(y)) {
        y <- x$gspcr_call$fit_measure
    } else {
        if (y != x$gspcr_call$fit_measure) {
            # Define the message
            message_wrongy <- paste0(y, " was not used as the fit measure in the CV procedure. If you want to plot the scores on the fit measure ", y, ", run cv_gspcr again and change the `fit_measure` argument to ", y, ".")
            # Return it
            warning(message_wrongy)
            # And use the fit_measure called by spcr
            y <- x$gspcr_call$fit_measure
        }
    }

    # Make scores from wide to long
    x.long <- reshape2::melt(
        data = x$scor,
        value.name = "fit_measure",
        varnames = c("npcs", "threshold")
    )

    # Make npcs a factor
    x.long$npcs <- factor(x.long$npcs)

    # Make the X-axis into factor if required
    if (discretize == TRUE) {
        x.long$threshold <- factor(
            x = round(x.long$threshold, 3),
            levels = round(x$thr, 3)
        )
    }

    # Add error bounds
    x.long$low <- reshape2::melt(data = x$scor_lwr)[, "value"]
    x.long$high <- reshape2::melt(x$scor_upr)[, "value"]

    # Make baseline plot
    store_plot <- x.long %>%
        ggplot2::ggplot(
            ggplot2::aes(
                x = .data[["threshold"]],
                y = .data[["fit_measure"]],
                group = .data$npcs,
                label = .data$npcs
            )
        ) +
        ggplot2::geom_line(colour = "gray") +
        ggplot2::labs(
            y = x$gspcr_call$fit_measure,
            x = x$gspcr_call$thr
        ) +
        ggplot2::theme_bw()

    # Add error bars if required
    if (errorBars == TRUE) {
        # Define the size of the error bars
        if (discretize == TRUE) {
            error_bars_size <- .2
        } else {
            error_bars_size <- 1 / 30 * diff(range(x.long[, 2]))
        }

        # Add the errors bars
        store_plot <- store_plot +
            ggplot2::geom_errorbar(
                ggplot2::aes(
                    ymin = .data$low,
                    ymax = .data$high
                ),
                width = error_bars_size,
                colour = "gray"
            )
    }

    # Add points if requested
    if (labels == FALSE) {
        store_plot <- store_plot +
            ggplot2::geom_point()
    }

    # Reverse y axis
    if (y_reverse == TRUE) {
        store_plot <- store_plot +
            ggplot2::scale_y_reverse()
    }

    # Avoid dropping empty levels if the X-axis is discrete
    if (discretize == TRUE) {
        store_plot <- store_plot +
            ggplot2::scale_x_discrete(drop = FALSE)
    }

    # Add labels if requested
    if (labels == TRUE) {
        store_plot <- store_plot +
            ggplot2::geom_text(colour = "black")
    }

    # Return plot
    store_plot
}