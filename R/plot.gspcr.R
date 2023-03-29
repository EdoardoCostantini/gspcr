#' Plot the gspcr solution path
#'
#' Produces a scatterplot showing the CV score obtained by cv.gspcr (Y-axis) with different threshold values (X-axis) for a different number of components (lines).
#'
#' @param gspcr.out The output of a cv.gspcr() call.
#' @param labels Logical value. FALSE hides the labels of the points indicating the number of components used. The default is TRUE.
#' @param errorBars Logical value. TRUE shows the errorBars for each point. The default is FALSE.
#' @param discretize Logical value. TRUE treats the X-axis as a discrete measure that facilitates comparing solution paths between different test types. The default is TRUE.
#' @param print Logical value. TRUE prints the plot when the function is called. The default is TRUE.
#' @details
#' The bounds defining the error bars are computed by cv.gspcr(). First, the K-fold cross-validation score of the statistic of interest (e.g., the F score, the MSE) is computed. Then, the standard deviation of the statistic across the K folds is computed. Finally, the bounds used for the error bars are computed by summing and subtracting this standard deviation to and from the K-fold cross-validation score of the statistic.
#' @return A scatterplot of ggplot class
#' @author Edoardo Costantini, 2023
#'
#' @export
plot.gspcr <- function(gspcr.out, labels = TRUE, errorBars = FALSE, discretize = TRUE, print = TRUE) {
    # Make scores from wide to long
    gspcr.out.long <- reshape2::melt(
        data = gspcr.out$scor,
        value.name = gspcr.out$gspcr.call$test,
        varnames = c("npcs", gspcr.out$gspcr.call$thrs)
    )

    # Make the X-axis into factor if required
    if(discretize == TRUE){
        gspcr.out.long[, gspcr.out$gspcr.call$thrs] <- factor(
            x = round(gspcr.out.long[, gspcr.out$gspcr.call$thrs], 3),
            levels = round(gspcr.out$thr, 3)
        )
    }

    # Add error bounds
    gspcr.out.long$low <- reshape2::melt(data = gspcr.out$scor.lwr)[, "value"]
    gspcr.out.long$high <- reshape2::melt(gspcr.out$scor.upr)[, "value"]

    # Make baseline plot
    store_plot <- gspcr.out.long %>%
        ggplot2::ggplot(
            ggplot2::aes(
                x = .data[[gspcr.out$gspcr.call$thrs]],
                y = .data[[gspcr.out$gspcr.call$test]],
                group = factor(npcs),
                label = factor(npcs)
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
                    ymin = low,
                    ymax = high
                ),
                width = .2
            )
    }

    # Return plot
    store_plot
}