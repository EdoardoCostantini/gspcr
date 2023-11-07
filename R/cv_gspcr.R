#' Cross-validation of Generalized Principal Component Regression
#'
#' Use K-fold cross-validation to decide on the number of principal components and the threshold value for GSPCR.
#'
#' @param dv numeric vector or factor of dependent variable values
#' @param ivs \eqn{n \times p} data.frame of independent variables (factors allowed)
#' @param fam character vector of length 1 storing the description of the error distribution and link function to be used in the model
#' @param thrs character vector of length 1 storing the type of threshold to be used (see below for available options)
#' @param nthrs numeric vector of length 1 storing the number of threshold values to be used
#' @param npcs_range numeric vector defining the numbers of principal components to be used
#' @param K numeric vector of length 1 storing the number of folds for the K-fold cross-validation procedure
#' @param fit_measure character vector of length 1 indicating the type of fit measure to be used in the cross-validation procedure
#' @param max_features numeric vector of length 1 indicating the maximum number of features that can be selected
#' @param min_features numeric vector of length 1 indicating the minimum number of features that should be selected
#' @param oneSE logical value indicating whether the results with the 1se rule should be saved
#' @param save_call logical value indicating whether the call should be saved and returned in the results
#' @details
#' The variables in \code{ivs} do not need to be standardized beforehand as the function handles scaling appropriately based on the measurement levels of the data.
#'
#' The \code{fam} argument is used to define which model will be used when regressing the dependent variable on the principal components:
#' - \code{gaussian}: fits a linear regression model (continuous dv)
#' - \code{binomial}: fits a logistic regression model (binary dv)
#' - \code{poisson}: fits a poisson regression model (count dv)
#' - \code{baseline}: fits a baseline-category logit model (nominal dv, using [nnet::multinom()])
#' - \code{cumulative}: fits a proportional odds logistic regression (ordinal dv, using [MASS::polr()])
#'
#' The \code{thrs} argument defines the bivariate association-threshold measures used to determine the active set of predictors for a SPCR analysis.
#' The following association measures are supported (measurement levels allowed reported between brackets):
#' - \code{LLS}: simple GLM regression likelihoods (any dv with any iv)
#' - \code{PR2}: Cox and Snell generalized R-squared is computed for the GLMs between \code{dv} and every column in \code{ivs}. Then, the square root of these values is used to obtain the threshold values. For more information about the computation of the Cox and Snell R2 see the help file for [gspcr::cp_gR2()]. When using this measure for simple linear regressions (with continuous \code{dv} and \code{ivs}) is equivalent to the regular R-squared. Therefore, it can be thought of as equivalent to the bivariate correlations between \code{dv} and \code{ivs}. (any dv with any iv)
#' - \code{normalized}: normalized correlation based on [superpc::superpc.cv()] (continuous dv with continuous ivs)
#'
#' The \code{fit_measure} argument defines which fit measure should be used within the cross-validation procedure.
#' The supported measures are:
#' - \code{F}: F-statistic computed with [cp_F()] (continuous dv)
#' - \code{LRT}: likelihood-ratio test statistic computed with [cp_LRT()] (any dv)
#' - \code{AIC}: Akaike's information criterion computed with [cp_AIC()] (any dv)
#' - \code{BIC}: bayesian information criterion computed with [cp_BIC()] (any dv)
#' - \code{PR2}: Cox and Snell generalized R-squared computed with [cp_gR2()] (any dv)
#' - \code{MSE}: Mean squared error compute with [MLmetrics::MSE()] (continuous dv)
#'
#' Details regarding the 1 standard error rule implemented here can be found in the documentation for the function [gspcr::cv_choose()].
#' @return
#' Object of class \code{gspcr}, which is a list containing:
#' - \code{solution}: a list containing the number of PCs that was selected (Q), the threshold value used, and the resulting active set for both the \code{standard} and \code{oneSE} solutions
#' - \code{sol_table}: data.frame reporting the threshold number, value, and the number of PCs identified by the procedure
#' - \code{thr}: vector of threshold values of the requested type used for the K-fold cross-validation procedure
#' - \code{thr_cv}: numeric vector of length 1 indicating the threshold number that was selected by the K-fold cross-validation procedure using the default decision rule
#' - \code{thr_cv_1se}: numeric vector of length 1 indicating the threshold number that was selected by the K-fold cross-validation procedure using the 1-standard-error rule
#' - \code{Q_cv}: numeric vector of length 1 indicating the number of PCs that was selected by the K-fold cross-validation procedure using the default decision rule
#' - \code{Q_cv_1se}: numeric vector of length 1 indicating the number of PCs that was selected by the K-fold cross-validation procedure using the 1-standard-error rule
#' - \code{scor}: \eqn{npcs \times nthrs} matrix of fit-measure scores averaged across the K folds
#' - \code{scor_lwr}: \eqn{npcs \times nthrs} matrix of fit-measure score lower bounds averaged across the K folds
#' - \code{scor_upr}: \eqn{npcs \times nthrs} matrix of fit-measure score upper bounds averaged across the K folds
#' - \code{pred_map}: matrix of \eqn{p \times nthrs} logical values indicating which predictors were active for every threshold value used
#' - \code{gspcr_call}: the function call
#'
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
#' save_call <- TRUE
#'
#' # Example usage
#' out_cont <- cv_gspcr(
#'   dv = GSPCRexdata$y$cont,
#'   ivs = GSPCRexdata$X$cont,
#'   fam = "gaussian",
#'   nthrs = 5,
#'   npcs_range = 1:3,
#'   K = 3,
#'   fit_measure = "F",
#'   thrs = "normalized",
#'   min_features = 1,
#'   max_features = ncol(GSPCRexdata$X$cont),
#'   oneSE = TRUE,
#'   save_call = TRUE
#' )
#'
#' @export
cv_gspcr <- function(
    dv,
    ivs,
    fam = c("gaussian", "binomial", "poisson", "baseline", "cumulative")[1],
    thrs = c("LLS", "PR2", "normalized")[1],
    nthrs = 10L,
    npcs_range = 1L:3L,
    K = 5,
    fit_measure = c("F", "LRT", "AIC", "BIC", "PR2", "MSE")[1],
    max_features = ncol(ivs),
    min_features = 1,
    oneSE = TRUE,
    save_call = TRUE) {
  # If ivs is not a data.frame make it one
  if (is.matrix(ivs)) {
    ivs <- as.data.frame(ivs, stringsAsFactors = TRUE)
  }

  # Perform other input checks
  if (is.factor(dv)) {
    dv <- droplevels(dv)
  }
  ivs <- check_constants(ivs)
  ivs <- check_factors(ivs)
  check_fam(fam)
  check_thrs(thrs)
  check_nthrs(nthrs)
  npcs_range <- check_npcs_range(npcs_range, ivs)
  check_K(K)
  check_fit_measure(fit_measure)
  max_features <- check_max_features(max_features, ivs)
  check_min_features(min_features, ivs)

  # Save the call if requested
  if (save_call) {
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
      oneSE = oneSE,
      save_call = save_call
    )
  } else {
    gspcr_call <- NULL
  }

  # Sample size
  n <- nrow(ivs)

  # Create an empty object to store possible errors and warnings
  errors <- NULL
  warnings_list <- NULL

  if (thrs == "LLS") {
    ascores <- tryCatch(
      withCallingHandlers(
        # The expression to evaluate.
        expr = {
          cp_thrs_LLS(
            dv = dv,
            ivs = ivs,
            fam = fam
          )
        },

        # The warning handler.
        warning = function(w) {
          # Append to warning object
          warnings_list <<- c(
            warnings_list,
            # Define a meaningful error message
            paste0(
              "WARNING IN ASSOCIATION MEASURE COMPUTATION\n",
              "One or more computations of the bivariate association measures resulted in these warnings:\n",
              w$message,
              "\n\n"
            )
          )

          # Prevent the warning from being printed.
          invokeRestart("muffleWarning")
        }
      )
    )
  }

  # Compute association measures

  if (thrs == "PR2") {
    ascores <- tryCatch(
      withCallingHandlers(
        # The expression to evaluate.
        expr = {
          cp_thrs_PR2(
            dv = dv,
            ivs = ivs,
            fam = fam
          )
        },

        # The warning handler.
        warning = function(w) {
          # Append to warning object
          warnings_list <<- c(
            warnings_list,
            # Define a meaningful error message
            paste0(
              "WARNING IN ASSOCIATION MEASURE COMPUTATION\n",
              "One or more computations of the bivariate association measures resulted in these warnings:\n",
              w$message,
              "\n\n"
            )
          )

          # Prevent the warning from being printed.
          invokeRestart("muffleWarning")
        }
      )
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
  lower <- stats::quantile(ascores, 1 - (max_features / ncol(ivs)), na.rm = TRUE)
  upper <- stats::quantile(ascores, 1 - (min_features / ncol(ivs)), na.rm = TRUE)

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
    # Create fold data
    if (length(unique(part)) != 1) {
      Xtr <- ivs[part != k, , drop = FALSE]
      Xva <- ivs[part == k, , drop = FALSE]
      ytr <- dv[part != k]
      yva <- dv[part == k]
    } else {
      Xtr <- ivs
      Xva <- ivs
      ytr <- dv
      yva <- dv
    }

    # Loop over threshold values
    for (thr in 1:nthrs_eff) {
      # thr <- 1
      # Define the active set of predictors based on the current threshold value
      aset <- pred_map[, thr]

      # If there is more than 1 active variable
      if (sum(aset) > 1) {
        # Check how many components are available (effective number)
        Q_max_eff <- min(sum(aset), max(npcs_range))
        Q_min_eff <- min(sum(aset), min(npcs_range))

        # Replace max and min in range on npcs
        npcs_range_eff <- npcs_range[npcs_range <= Q_max_eff & npcs_range >= Q_min_eff]

        # Compute PC scores
        pca_mix_out <- tryCatch(
          expr = {
            pca_mix(
              X_tr = Xtr[, aset, drop = FALSE],
              X_va = Xva[, aset, drop = FALSE],
              npcs = Q_max_eff
            )
          },
          error = function(e) {
            # Store error.
            errors <<- c(
              errors,
              paste0(
                "ERROR IN K-FOLD LOOP\n",
                "Fold: ", k, "; Threshold: ", thr, "; resulted in the following error:\n",
                "\"", e$message, "\"\n",
                "The values of ", fit_measure, " using threshold number", thr, "could not be computed and were set to NA value so that they will be ignored.\n\n"
              )
            )

            # Return the error
            return(e)
          }
        )

        # If an error occured in the pc_score computation, skip this threshold
        if ("error" %in% class(pca_mix_out)) next

        # Compute the fit measures for the additive PCRs
        for (q in npcs_range_eff) {
          # q <- 1
          # Compute the fit measure value
          map_kfcv[q, thr, k] <- tryCatch(
            withCallingHandlers(
              expr = {
                cp_validation_fit(
                  y_train = ytr,
                  y_valid = yva,
                  X_train = pca_mix_out$PC_tr[, 1:q, drop = FALSE],
                  X_valid = pca_mix_out$PC_va[, 1:q, drop = FALSE],
                  fam = fam,
                  fit_measure = fit_measure
                )
              },

              # If there is a warning, append it to the list of warnings
              warning = function(w) {
                # Store warning.
                warnings_list <<- c(
                  warnings_list,
                  paste0(
                    "WARNING IN K-FOLD LOOP\n",
                    "Fold: ", k, "; Threshold: ", thr, "; npcs: ", q, "; resulted in the following warning:\n",
                    "\"", w$message, "\"\n"
                  )
                )

                # Prevent the warning from being printed.
                invokeRestart("muffleWarning")
              }
            ),

            # If there is an error, append it and return NA as a fit value
            error = function(e) {
              # Store error.
              errors <<- c(
                errors,
                paste0(
                  "ERROR IN K-FOLD LOOP\n",
                  "Fold: ", k, "; Threshold: ", thr, "; npcs: ", q, "; resulted in the following error:\n",
                  "\"", e$message, "\"\n",
                  "The value of ", fit_measure, " that could not be computed was replaced by an NA value.\n\n"
                )
              )

              # Return NA to salvage general K-fold cross-validation
              return(NA)
            }
          )
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

  # Make a solution table
  sol_table <- data.frame(
    thr_value = c(
      standard = thrs_values[cv_sol$default[2]],
      oneSE = thrs_values[cv_sol$oneSE[2]]
    ),
    thr_number = c(
      standard = as.numeric(cv_sol$default[2]),
      oneSE = as.numeric(cv_sol$oneSE[2])
    ),
    Q = c(
      standard = as.numeric(cv_sol$default[1]),
      oneSE = as.numeric(cv_sol$oneSE[1])
    )
  )

  # Return
  out <- list(
    solution = list(
      standard = list(
        Q = sol_table["standard", "Q"],
        threshold = sol_table["standard", "thr_value"],
        active_set = names(
          which(
            pred_map[, sol_table["standard", "thr_number"]]
          )
        )
      ),
      oneSE = list(
        Q = sol_table["oneSE", "Q"],
        threshold = sol_table["oneSE", "thr_value"],
        active_set = names(
          which(
            pred_map[, sol_table["oneSE", "thr_number"]]
          )
        )
      )
    ),
    sol_table = sol_table,
    thr = thrs_values,
    thr_cv = sol_table["standard", "thr_value"],
    thr_cv_1se = sol_table["oneSE", "thr_value"],
    Q_cv = sol_table["standard", "Q"],
    Q_cv_1se = sol_table["oneSE", "Q"],
    scor = scor_list$scor,
    scor_lwr = scor_list$scor_lwr,
    scor_upr = scor_list$scor_upr,
    pred_map = pred_map,
    gspcr_call = gspcr_call
  )

  # Assign class to object
  class(out) <- c("gspcrcv", "list")

  # Print warnings and errors as messages
  if (!is.null(warnings_list)) {
    message(
      warnings_list
    )
  }
  if (!is.null(errors)) {
    message(
      errors
    )
  }

  # Return gspcr object
  return(out)
}