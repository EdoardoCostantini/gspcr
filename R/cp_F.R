#' Compute F statistic
#'
#' Computes the F statistic comparing two nested models.
#'
#' @param y numeric vector storing the observed values on the dependent variable
#' @param y_hat_restricted numeric vector storing the predicted values on \code{y} based on the restricted model
#' @param y_hat_full numeric vector storing the predicted values on \code{y} based on the full model
#' @param n numeric vector of length 1 storing the sample size used to train the models
#' @param p_restricted numeric vector of length 1 storing the number of predictors involved in training the restricted model
#' @param p_full numeric vector of length 1 storing the number of predictors involved in training the full model
#' @details
#' Note that:
#' - The full model is always the model with more estimated parameters, the model with more predictor variables.
#' - The restricted model is the model with fewer estimated parameters. 
#' - The restricted model must be nested within the full model.
#' @return numeric vector of length 1 storing the F-statistic
#' @author Edoardo Costantini, 2023
#' @examples
#' # Null vs full model
#' lm_n <- lm(mpg ~ 1, data = mtcars) # Fit a null model
#' lm_f <- lm(mpg ~ cyl + disp, data = mtcars) # Fit a full model
#' f_M <- cp_F(
#'     y = mtcars$mpg,
#'     y_hat_restricted = predict(lm_n),
#'     y_hat_full = predict(lm_f),
#'     p_full = 2
#' )
#' 
#' # Simpler vs more complex model
#' lm_f_2 <- lm(mpg ~ cyl + disp + hp + drat + qsec, data = mtcars) # a more complex full model
#' f_change_M <- cp_F(
#'     y = mtcars$mpg,
#'     y_hat_restricted = predict(lm_f),
#'     y_hat_full = predict(lm_f_2),
#'     p_restricted = 2,
#'     p_full = 5
#' )
#' 
#' @export
cp_F <- function(y, y_hat_restricted, y_hat_full, n = length(y), p_restricted = 0, p_full){

    # prediction error restricted model
    Er <- TSS <- sum((y - y_hat_restricted)^2)

    # prediction error full model
    Ef <- SSE <- sum((y - y_hat_full)^2)

    # Compute degrees of freedom for the restricted model
    dfR <- (n - p_restricted - 1)

    # Compute degrees of freedom for the full model
    dfF <- (n - p_full - 1)

    # Compute the f statistic
    Fstat <- ((Er - Ef) / (dfR - dfF)) / (Ef / dfF)

    # Return
    as.numeric(Fstat)

}