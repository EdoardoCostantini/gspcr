#' Cross-validation of Generalized Principal Component Regression
#'
#' Use K-fold cross-validation to decide on the number of principal components and the threshold value for GSPCR.
#'
#' @param dv Vector of dependent variable values
#' @param ivs Matrix of predictor values
#' @param fam GLM framework for the dv
#' @param thrs Type of threshold to be used
#' @param nthrs Number of threshold values to be used
#' @param maxnpcs = Maximum number of principal components to be used
#' @param K Number of folds for the K-fold cross-validation procedure
#' @param fit_measure Type of measure to cross-validate
#' @param max.features Maximum number of features that can be selected
#' @param min.features Minimum number of features that can be selected
#' @param oneSE Whether the results with the 1SE rule should be stored
#' @details
#' This function does such and such.
#' @return Returns an object of class gspcr.
#' @author Edoardo Costantini, 2023
#' @references
#'
#' Such, S. (2006). Such and such. Journal such and such, 101(473), 119-137.
#'
#' @export
cv_gspcr <- function(
  dv, 
  ivs, 
  fam = "gaussian",
  thrs = c("LLS", "pseudoR2", "normalized")[1],
  nthrs = 10,
  maxnpcs = 3,
  K = 5,
  fit_measure = c("LRT", "F", "MSE")[2],
  max.features = ncol(ivs),
  min.features = 5,
  oneSE = TRUE
  ) {

  # Example inputs
  # dv <- mtcars[, 1]
  # ivs <- mtcars[, -1]
  # thrs = c("LLS", "pseudoR2", "normalized")[3]
  # nthrs = 5
  # fam <- c("gaussian", "binomial", "poisson")[1]
  # maxnpcs <- 10
  # K = 2
  # fit_measure = c("LRT", "F", "MSE")[2]
  # max.features = ncol(ivs)
  # min.features = 1
  # oneSE = TRUE

  # Save the call
  gspcr.call <- list(
    dv = dv,
    ivs = ivs,
    fam = fam,
    thrs = thrs,
    nthrs = nthrs,
    maxnpcs = maxnpcs,
    K = K,
    fit_measure = fit_measure,
    max.features = max.features,
    min.features = min.features,
    oneSE = oneSE
  )

  # Sample size
  n <- nrow(ivs)

  # Fit null model
  glm0 <- stats::glm(dv ~ 1, family = fam)

  # Fit univariate models
  glm.fits <- lapply(1:ncol(ivs), function(j) {
    stats::glm(dv ~ ivs[, j], family = fam)
  })

  # Extract Log-likelihood values
  ll0 <- as.numeric(stats::logLik(glm0))
  lls <- sapply(glm.fits, function(m) as.numeric(stats::logLik(m)))

  # Create active sets based on threshold type

  if(thrs == "LLS"){

    # Use the logLikelihoods as bivariate association scores
    ascores <- lls

    # Give it good names
    names(ascores) <- colnames(ivs)

    # Define the upper and lower bounds of the association
    lower <- min(ascores)
    upper <- max(ascores)

  }

  if(thrs == "pseudoR2"){

    # Compute pseudo R-squared
    CNR2 <- 1 - exp(-2 / n * (lls - ll0))

    # Give it good names
    names(CNR2) <- colnames(ivs)

    # Make them correlation coefficients
    ascores <- sqrt(CNR2)

    # Define upper and lower bounds of the association
    lower <- stats::quantile(ascores, 1 - (max.features / ncol(ivs)))
    upper <- stats::quantile(ascores, 1 - (min.features / ncol(ivs)))

  }

  if (thrs == "normalized") {
    
    # Set objects to the required dimension
    x <- t(as.matrix(ivs))
    y <- dv
    featurenames <- colnames(ivs)

    # Empty
    s0.perc <- NULL

    # Sample size
    n <- length(y)

    # Compute vector of feature means
    xbar <- x %*% rep(1 / n, n)

    # Same as computing the row means
    cbind(xbar, rowMeans(x))

    # Compute the diagonal of the cross-product matrix between variables
    sxx <- ((x - as.vector(xbar))^2) %*% rep(1, n)

    # Compute the cross-product matrix between X and Y
    sxy <- (x - as.vector(xbar)) %*% (y - mean(y))

    # Total sum of squares
    syy <- sum((y - mean(y))^2)

    # Ratio of the two
    numer <- sxy / sxx

    # Compute sd?
    stdev <- sqrt((syy / sxx - numer^2) / (n - 2))

    # add "fudge"(?) to the denominator
    if (is.null(s0.perc)) {
      fudge <- stats::median(stdev)
    }
    if (!is.null(s0.perc)) {
      if (s0.perc >= 0) {
        fudge <- stats::quantile(stdev, s0.perc)
      }
      if (s0.perc < 0) {
        fudge <- 0
      }
    }

    # Ratio between numerator and stdev
    tt <- numer / (stdev + fudge)

    # Store the normalized correlation scores
    ascores <- abs(tt)[, 1]

    # Define upper and lower bounds of the normalized correlation
    lower <- stats::quantile(abs(ascores), 1 - (max.features / nrow(x)))
    upper <- stats::quantile(abs(ascores), 1 - (min.features / nrow(x)))

  }

  # Define threshold values
  thrs_values <- seq(from = lower, to = upper, length.out = nthrs)

  # Create a map of active predictors based on threshold values
  pred.map <- sapply(1:nthrs, function(a) ascores > thrs_values[a])

  # Use thresholds as names
  colnames(pred.map) <- round(thrs_values, 3)

  # If two thresholds are giving the same result reduce the burden
  pred.map <- pred.map[, !duplicated(t(pred.map))]

  # Get rid of thresholds that are keeping too few predictors
  pred.map <- pred.map[, colSums(pred.map) >= min.features]

  # Get rid of thresholds that are keeping too many predictors
  pred.map <- pred.map[, colSums(pred.map) <= max.features]

  # And update the effective number of the thresholds considered
  nthrs.eff <- ncol(pred.map)
  
  # Create an object to store k-fold cross-validation log-likelihoods
  map_kfcv <- array(
    dim = c(maxnpcs, nthrs.eff, K),
    dimnames = list(NULL, colnames(pred.map), NULL)
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
    for (thr in 1:nthrs.eff) {
      # thr <- 1
      # Define the active set of predictors based on the current threshold value
      aset <- pred.map[, thr]

      # If there is more than 1 active variable
      if (sum(aset) > 1) {

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

        # Check how many components are available (effective number)
        q.eff <- min(sum(aset), maxnpcs)

        # Select the available PC scores
        PC_tr.eff <- PC_tr[, 1:q.eff, drop = FALSE]
        PC_va.eff <- PC_va[, 1:q.eff, drop = FALSE]

        # Compute the F-statistic for the possible additive PCRs
        for (Q in 1:q.eff) {
          # Q <- 1

          # Train GLM model and baseline model
          glm_fit_tr <- stats::glm(ytr ~ PC_tr.eff[, 1:Q], family = fam)

          # Store the baseline GLM model
          glm_null_tr <- stats::glm(ytr ~ 1, family = fam)
          
          # Obtain prediction based on new data
          yhat_va <- cbind(1, PC_va.eff[, 1:Q]) %*% stats::coef(glm_fit_tr)

          # Obtain validation residuals
          r_va_mod <- (yva - yhat_va)
          r_va_null <- yva - mean(ytr)

          # Store the estimate of the sigma
          s_va_mod <- sqrt(sum(stats::resid(glm_fit_tr)^2) / (length(ytr))) # maximum likelihood version
          s_va_null <- sqrt(sum(stats::resid(glm_null_tr)^2) / (length(ytr))) # maximum likelihood version

          # Compute validation data log-likelihood under the null model
          loglik_va_null <- loglike_norm(r = r_va_null, s = s_va_null)

          # Compute validation data log-likelihood under the model
          loglik_va_mod <- loglike_norm(r = r_va_mod, s = s_va_mod)

          # Extract desired statistic
          if (fit_measure == "F") {
            # Compute residuals
            Er <- TSS <- sum((yva - mean(ytr))^2) # baseline prediction error
            Ef <- SSE <- sum((yva - yhat_va)^2) # model prediction error

            # Compute degrees of freedom
            dfR <- (n - 0 - 1) # for the restricted model
            dfF <- (n - Q - 1) # for the full model

            # Compute the f statistic
            Fstat <- ((Er - Ef) / (dfR - dfF)) / (Ef / dfF)

            # Store the F stats
            map_kfcv[Q, thr, k] <- Fstat
          }
          if (fit_measure == "LRT") {
            map_kfcv[Q, thr, k] <- 2 * (loglik_va_mod - loglik_va_null)
          }
          if (fit_measure == "AIC") {
            map_kfcv[Q, thr, k] <- 2 * (Q + 1 + 1) - 2 * loglik_va_mod
          }
          if (fit_measure == "BIC") {
            map_kfcv[Q, thr, k] <- log(length(r_va_mod)) * (Q + 1 + 1) - 2 * loglik_va_mod
          }
          if (fit_measure == "PR2") {
            map_kfcv[Q, thr, k] <- 1 - exp(-2 / length(r_va_mod) * (loglik_va_mod - loglik_va_null))
          }
          if (fit_measure == "MSE") {
            map_kfcv[Q, thr, k] <- MLmetrics::MSE(y_pred = yhat_va, y_true = yva)
          }
        }
      }
    }
  }

  # Average selected score across folds
  scor.list <- cv_collect(cv_array = map_kfcv, fit_measure = fit_measure)

  # Make a decision based on the CV measures
  cv_sol <- cv_choose(
    scor = scor.list$scor,
    scor.lwr = scor.list$scor.lwr,
    scor.upr = scor.list$scor.upr,
    K = K,
    fit_measure = fit_measure
  )

  # Return
  out <- list(
    thr         = thrs_values,
    thr.cv      = thrs_values[cv_sol$default[2]],
    thr.cv.1se  = thrs_values[cv_sol$oneSE[2]],
    Q.cv        = cv_sol$default[1],
    Q.cv.1se    = cv_sol$oneSE[1],
    scor        = scor.list$scor,
    scor.lwr    = scor.list$scor.lwr,
    scor.upr    = scor.list$scor.upr,
    pred.map    = pred.map,
    gspcr.call  = gspcr.call
  )

  # Assign class to object
  class(out) <- c("gspcrout", "list")

  # Return gspcr object
  return(out)
}