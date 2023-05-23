#' Cross-validation of Generalized Principal Component Regression
#'
#' Use K-fold cross-validation to decide on the number of principal components and the threshold value for GSPCR.
#'
#' @param dv Vector of dependent variable values
#' @param ivs Matrix of predictor values
#' @param fam GLM framework for the dv
#' @param thrs Type of threshold to be used
#' @param nthrs Number of threshold values to be used
#' @param npcs_range range of number of principal components to be used
#' @param K Number of folds for the K-fold cross-validation procedure
#' @param fit_measure Type of measure to cross-validate.
#' @param max_features Maximum number of features that can be selected
#' @param min_features Minimum number of features that can be selected
#' @param oneSE Whether the results with the 1SE rule should be stored
#' @details
#' The variables in `ivs` do not need to be standardized beforehand as the function handles scaling appropriately based on the measurement levels of the data.
#' Here we list the supported association-threshold measures to determine the active set of predictors for a SPCR analysis (the supported measurement levels for the variables involved is reported between brackets):
#' \itemize{
#'   \item \code{LLS} (any dv with any iv)
#'   \item \code{PR2} (any dv with any iv) - The Cox and Snell generalized R-squared is computed for the GLMs between \code{dv} and every column in \code{ivs}. Then, the square root of these values is used to obtain the threshold values. For more information about the computation of the Cox and Snell R2 see the help file for [gspcr::cp_gR2()]. When using this measure for simple linear regressions (with continuous \code{dv} and \code{ivs}) is equivalent to the regular R-squared. Therefore, it can be thought of as equivalent to the bivariate correlations between \code{dv} and \code{ivs}.
#'   \item \code{normalized} (continuous dv with continuous ivs)
#' }
#' Here we list the supported fit measures to be used within the cross-validation procedure:
#' \itemize{
#'   \item \code{F} - (continuous dv)
#'   \item \code{LRT} - (any dv)
#'   \item \code{AIC} - (any dv)
#'   \item \code{BIC} - (any dv)
#'   \item \code{PR2} - (any dv)
#'   \item \code{MSE} - (continuous dv)
#' }
#' @return Returns an object of class \code{gspcr}.
#' @author Edoardo Costantini, 2023
#' @references
#' 
#' Bair, E., Hastie, T., Paul, D., & Tibshirani, R. (2006). Prediction by supervised principal components. Journal of the American Statistical Association, 101(473), 119-137.
#'
#' @examples
#' # Example input values
#' dv <- mtcars[, 1]
#' ivs <- mtcars[, -1]
#' thrs <- "PR2"
#' nthrs <- 5
#' fam <- "gaussian"
#' npcs_range <- 1:3
#' K <- 3
#' fit_measure <- "F"
#' max_features <- ncol(ivs)
#' min_features <- 1
#' oneSE <- TRUE
#'
#' # Example usage
#' out_cont <- cv_gspcr(
#'    dv = GSPCRexdata$y$cont,
#'    ivs = GSPCRexdata$X$cont,
#'    fam = "gaussian",
#'    nthrs = 5,
#'    npcs_range = 1:3,
#'    K = 3,
#'    fit_measure = "F",
#'    thrs = "normalized",
#'    min_features = 1,
#'    max_features = ncol(GSPCRexdata$X$cont),
#'    oneSE = TRUE
#' )
#'
#' @export
cv_gspcr <- function(
  dv, 
  ivs, 
  fam = "gaussian",
  thrs = c("LLS", "PR2", "normalized")[1],
  nthrs = 10,
  npcs_range = 1:3,
  K = 5,
  fit_measure = c("F", "LRT", "AIC", "BIC", "PR2", "MSE")[1],
  max_features = ncol(ivs),
  min_features = 5,
  oneSE = TRUE
  ) {

  # Save the call
  gspcr_call <- list(
    dv = dv,
    ivs = ivs,
    fam = fam,
    thrs = thrs,
    nthrs = nthrs,
    npcs_range = npcs_range,
    K = K,
    fit_measure = fit_measure,
    max_features = max_features,
    min_features = min_features,
    oneSE = oneSE
  )

  # Sample size
  n <- nrow(ivs)

  # Compute association measures
  if (thrs == "LLS") {
    ascores <- cp_thrs_LLS(
      dv = dv,
      ivs = ivs,
      fam = fam
    )
  }

  if (thrs == "PR2") {
    ascores <- cp_thrs_PR2(
      dv = dv,
      ivs = ivs,
      fam = fam
    )
  }

  if (thrs == "normalized") {
    ascores <- cp_thrs_NOR(
      dv = dv,
      ivs = ivs,
      s0_perc = NULL
    )
  }

  # Define upper and lower bounds of the association
  lower <- stats::quantile(ascores, 1 - (max_features / ncol(ivs)))
  upper <- stats::quantile(ascores, 1 - (min_features / ncol(ivs)))

  # Define threshold values
  thrs_values <- seq(from = lower, to = upper, length.out = nthrs)

  # Create a map of active predictors based on threshold values
  pred_map <- sapply(1:nthrs, function(a) ascores > thrs_values[a])

  # Use thresholds as names
  colnames(pred_map) <- round(thrs_values, 3)

  # If two thresholds are giving the same active set reduce the burden
  pred_map <- pred_map[, !duplicated(t(pred_map))]

  # Get rid of thresholds that are keeping too few predictors
  pred_map <- pred_map[, colSums(pred_map) >= min_features]

  # Get rid of thresholds that are keeping too many predictors
  pred_map <- pred_map[, colSums(pred_map) <= max_features]

  # And update the effective number of the thresholds considered
  nthrs_eff <- ncol(pred_map)

  # Create an object to store k-fold cross-validation fit measures
  map_kfcv <- array(
    dim = c(max(npcs_range), nthrs_eff, K),
    dimnames = list(NULL, colnames(pred_map), NULL)
  )

  # Create a fold partitioning object
  part <- sample(rep(1:K, ceiling(nrow(ivs) / K)))[1:nrow(ivs)]

  # Loop over K folds
  for (k in 1:K) {
    # k <- 1

    # Create fold data:
    Xtr <- ivs[part != k, , drop = FALSE]
    Xva <- ivs[part == k, , drop = FALSE]
    ytr <- dv[part != k]
    yva <- dv[part == k]

    # Loop over threshold values
    for (thr in 1:nthrs_eff) {
      # thr <- 1
      # Define the active set of predictors based on the current threshold value
      aset <- pred_map[, thr]

      # If there is more than 1 active variable
      if (sum(aset) > 1) {

        # Check how many components are available (effective number)
        q_max_eff <- min(sum(aset), max(npcs_range))
        q_min_eff <- min(sum(aset), min(npcs_range))

        # Replace max and min in range
        npcs_range_eff <- npcs_range[npcs_range <= q_max_eff & npcs_range >= q_min_eff]

        if(all(sapply(ivs, is.numeric))){

          # Scale Xs
          Xtr_thr <- scale(Xtr[, aset], center = TRUE, scale = TRUE)
          Xva_thr <- scale(Xva[, aset],
            center = attributes(Xtr_thr)$`scaled:center`,
            scale = attributes(Xtr_thr)$`scaled:scale`
          )

          # Perform PCA on the training data
          svd_Xtr <- svd(Xtr_thr)

          # Project training and validation data on the PCs
          PC_tr <- Xtr_thr %*% svd_Xtr$v
          PC_va <- Xva_thr %*% svd_Xtr$v

        } else {

          # Perform PCAmix
          pca_mix_out <- pca_mix(
            X_tr = Xtr[, aset],
            X_va = Xva[, aset],
            npcs = q_max_eff
          )

          # Extract objects of interest
          PC_tr <- pca_mix_out$PC_tr
          PC_va <- pca_mix_out$PC_va

        }

        # Select the available PC scores
        PC_tr_eff <- PC_tr[, 1:q_max_eff, drop = FALSE]
        PC_va_eff <- PC_va[, 1:q_max_eff, drop = FALSE]

        # Compute the fit measures for the possible additive PCRs
        for (Q in npcs_range_eff) {
          # Q <- 1

          # Estimate new data log-likelihoods under the model of interest
          mod_out <- LL_newdata(
            y_train = ytr,
            y_valid = yva,
            X_train = PC_tr_eff[, 1:Q, drop = FALSE],
            X_valid = PC_va_eff[, 1:Q, drop = FALSE],
            fam = fam
          )

          # Estimate new data log-likelihoods under the null model
          null_out <- LL_newdata(
            y_train = ytr,
            y_valid = yva,
            X_train = 1,
            X_valid = 1,
            fam = fam
          )

          # Extract desired statistic
          if (fit_measure == "F") {
            # Compute F statistic with your function
            map_kfcv[Q, thr, k] <- cp_F(
              y = yva,
              y_hat_restricted = null_out$yhat_va,
              y_hat_full = mod_out$yhat_va,
              n = length(yva),
              p_restricted = 0,
              p_full = Q
            )
          }
          if (fit_measure == "LRT") {
            map_kfcv[Q, thr, k] <- cp_LRT(
              ll_restricted = null_out$LL,
              ll_full = mod_out$LL
            )
          }
          if (fit_measure == "AIC") {
            map_kfcv[Q, thr, k] <- cp_AIC(
              ll = mod_out$LL,
              k = Q + 1 + 1
            )
          }
          if (fit_measure == "BIC") {
            map_kfcv[Q, thr, k] <- cp_BIC(
              ll = mod_out$LL,
              n = length(yva),
              k = Q + 2
            )
          }
          if (fit_measure == "PR2") {
            map_kfcv[Q, thr, k] <- cp_gR2(
              ll_n = null_out$LL,
              ll_f = mod_out$LL,
              n = length(yva)
            )
          }
          if (fit_measure == "MSE") {
            map_kfcv[Q, thr, k] <- MLmetrics::MSE(y_pred = mod_out$yhat_va, y_true = yva)
          }
        }
      }
    }
  }

  # Average selected score across folds
  scor_list <- cv_average(cv_array = map_kfcv, fit_measure = fit_measure)

  # Make a decision based on the CV measures
  cv_sol <- cv_choose(
    scor = scor_list$scor,
    scor_lwr = scor_list$scor_lwr,
    scor_upr = scor_list$scor_upr,
    K = K,
    fit_measure = fit_measure
  )

  # Return
  out <- list(
    thr         = thrs_values,
    thr_cv      = thrs_values[cv_sol$default[2]],
    thr_cv_1se  = thrs_values[cv_sol$oneSE[2]],
    Q_cv        = cv_sol$default[1],
    Q_cv_1se    = cv_sol$oneSE[1],
    scor        = scor_list$scor,
    scor_lwr    = scor_list$scor_lwr,
    scor_upr    = scor_list$scor_upr,
    pred_map    = pred_map,
    gspcr_call  = gspcr_call
  )

  # Assign class to object
  class(out) <- c("gspcrout", "list")

  # Return gspcr object
  return(out)
}