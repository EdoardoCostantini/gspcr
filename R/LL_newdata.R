#' Log-Likelihood for new data
#'
#' Given training and validation datasets, this function returns the log-likelihood of unobserved data under the model trained on the training data.
#'
#' @param y_train Vector of DV values in the training dataset.
#' @param y_valid Vector of DV values in the validation dataset.
#' @param X_train Matrix of IV values in the training dataset. Can also be set to 1 to obtain the log-likelihood of the new data under the null model.
#' @param X_valid Matrix of IV values in the validation dataset. If \code{X_train} is set to 1 to obtain the log-likelihood of the new data under the null model, \code{X_valid} is ignored.
#' @param fam GLM framework for the dv.
#' @details
#' This function trains a GLM regressing \code{y_train} on \code{X_train} using as link function what is specified in \code{fam}. Then, it computes the predictions for the validation data based on the trained model on the scale of the linear predictors (e.g., logit). The likelihood of the validation under the model is returned.
#' 
#' @return A list of objects.
#' @author Edoardo Costantini, 2023
#' @references
#'
#' Such, S. (2006). Such and such. Journal such and such, 101(473), 119-137.
#'
#' @export
LL_newdata <- function(y_train, y_valid, X_train, X_valid, fam) {

  ## Example inputs
  # y_train = as.matrix(mtcars[1:20, 1])
  # y_valid = as.matrix(mtcars[-c(1:20), 1])
  # X_train = 1
  # X_valid = 1
  # fam = "gaussian"

  ## Body

  # Collect data in data.frames
  train <- data.frame(y = y_train, X = X_train)
  valid <- data.frame(y = y_valid, X = X_valid)

  # Define formula
  if (length(X_train) == 1) {
    # Null model
    glm_formula <- stats::as.formula("y ~ 1")
  } else {
    # Some model of interest
    glm_formula <- stats::as.formula(
      paste0("y ~ ", paste0(colnames(train)[-1], collapse = " + "))
    )
  }

  # Train GLM model
  glm_fit_tr <- stats::glm(glm_formula, data = train, family = fam)

  # Obtain prediction for new data
  yhat_va <- stats::predict(
    object = glm_fit_tr,
    newdata = valid,
    type = "link"
  )

  # Evaluate the log-likelihood of new data under this model
  if (fam == "binomial") {
    LL_va_mod <- LL_binomial(
      y = y_valid,
      lgt = yhat_va
    )
  }
  if (fam == "gaussian") {
    LL_va_mod <- LL_gaussian(
      y = y_valid,
      y_hat = yhat_va,
      mod = glm_fit_tr
    )
  }

  # Return
  list(
    LL = LL_va_mod,
    yhat_va = yhat_va,
    mod = glm_fit_tr
  )
}