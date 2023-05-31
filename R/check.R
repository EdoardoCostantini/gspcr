# Project:   gspcr
# Objective: Script containing functions to check correct inputs specifications
# Author:    Edoardo Costantini
# Created:   2023-05-31
# Modified:  2023-05-31
# Notes:

# Argument: fam ----------------------------------------------------------------

check_fam <- function(fam) {
    # Is the requested fam in the list of possible families?
    is_fam_in <- fam %in% c("gaussian", "binomial", "poisson", "baseline", "cumulative")

    # Throw error
    if (!is_fam_in) {
        stop(
            paste0(
                "argument 'fam' is misspecified. '",
                fam,
                "' is either misspelled or not a supported familiy."
            ),
            call. = FALSE
        )
    }
}

# Argument: thrs ---------------------------------------------------------------

check_thrs <- function(thrs) {
    # Is the requested thrs in the list of possible types?
    is_thrs_in <- thrs %in% c("LLS", "PR2", "normalized")[1]

    # Throw error
    if (!is_thrs_in) {
        stop(
            paste0(
                "argument 'thrs' is misspecified. '",
                thrs,
                "' is either misspelled or not a supported association measure. See help file for supported measures."
            ),
            call. = FALSE
        )
    }
}

# Argument: nthrs --------------------------------------------------------------

check_nthrs <- function(nthrs) {
    # Is the requested number of thresholds a positive number
    is_positive <- nthrs > 0
    if (!is_positive) {
        stop(
            paste0(
                "argument 'nthrs' is misspecified. '",
                nthrs,
                "' is not positive."
            ),
            call. = FALSE
        )
    }

    # Is the requested number of thresholds an integer number
    is_integer <- nthrs %% 1 == 0
    if (!is_integer) {
        stop(
            paste0(
                "argument 'nthrs' is misspecified. '",
                nthrs,
                "' is not an integer value."
            ),
            call. = FALSE
        )
    }
}

# Argument: npcs_range ---------------------------------------------------------

check_npcs_range <- function(npcs_range, ivs) {
    # Is the requested number of thresholds a positive number
    is_positive <- all(npcs_range > 0)
    if (!is_positive) {
        stop(
            paste0(
                "argument 'npcs_range' is misspecified. One or more values in '",
                nthrs,
                "' are not positive."
            ),
            call. = FALSE
        )
    }

    # Is the requested number of thresholds an integer number
    is_integer <- all(npcs_range %% 1 == 0)
    if (!is_integer) {
        stop(
            paste0(
                "argument 'npcs_range' is misspecified. One or more values in '",
                npcs_range,
                "' are not an integer value."
            ),
            call. = FALSE
        )
    }

    # The number of npcs requested is less than the total number of columns
    is_less_than_p <- max(npcs_range) <= ncol(ivs)
    if (!is_less_than_p) {
        stop(
            paste0(
                "argument 'npcs_range' is misspecified. You cannot ask for more PCs than the number of columns in the input data 'ivs'. Get rid of (", paste(npcs_range[npcs_range > ncol(ivs)], collapse = ", "), ") from the range of PCs requested."
            ),
            call. = FALSE
        )
    }
}

# Argument: K ------------------------------------------------------------------

check_K <- function(K) {
    # Is the requested number of folds a positive number
    is_positive <- K > 0
    if (!is_positive) {
        stop(
            paste0(
                "argument 'K' is misspecified. '",
                K,
                "' is not bigger than 0."
            ),
            call. = FALSE
        )
    }

    # Is the requested number of folds an integer number
    is_integer <- K %% 1 == 0
    if (!is_integer) {
        stop(
            paste0(
                "argument 'K' is misspecified. '",
                K,
                "' is not an integer value."
            ),
            call. = FALSE
        )
    }
}

# Argument: fit_measure --------------------------------------------------------

check_fit_measure <- function(fit_measure) {
    # Is the requested fit_measure in the list of possible fit measures?
    is_fit_measure_in <- fit_measure %in% c("F", "LRT", "AIC", "BIC", "PR2", "MSE")

    # Throw error
    if (!is_fit_measure_in) {
        stop(
            paste0(
                "argument 'fit_measure' is misspecified. '",
                fit_measure,
                "' is either misspelled or not a supported fit measure."
            ),
            call. = FALSE
        )
    }
}

# Argument: max_features -------------------------------------------------------

check_max_features <- function(max_features, ivs) {
    # Is the requested number of thresholds a positive number
    is_positive <- all(max_features > 0)
    if (!is_positive) {
        stop(
            paste0(
                "argument 'max_features' is misspecified. One or more values in '",
                max_features,
                "' are not positive."
            ),
            call. = FALSE
        )
    }

    # Is the requested number of thresholds an integer number
    is_integer <- all(max_features %% 1 == 0)
    if (!is_integer) {
        stop(
            paste0(
                "argument 'max_features' is misspecified. One or more values in '",
                max_features,
                "' are not an integer value."
            ),
            call. = FALSE
        )
    }

    # The number of features requested is less than or equal to the total number of columns
    is_less_than_p <- max(max_features) <= ncol(ivs)
    if (!is_less_than_p) {
        stop(
            paste0(
                "argument 'max_features' is misspecified. You cannot ask for more features than the number of columns in the input data 'ivs'. Get rid of (", paste(max_features[max_features > ncol(ivs)], collapse = ", "), ") from the range of PCs requested."
            ),
            call. = FALSE
        )
    }
}

# Argument: min_features -------------------------------------------------------

check_min_features <- function(min_features, ivs) {
    # Is the requested number of thresholds a positive number
    is_positive <- all(min_features > 0)
    if (!is_positive) {
        stop(
            paste0(
                "argument 'min_features' is misspecified. One or more values in '",
                min_features,
                "' are not positive."
            ),
            call. = FALSE
        )
    }

    # Is the requested number of thresholds an integer number
    is_integer <- all(min_features %% 1 == 0)
    if (!is_integer) {
        stop(
            paste0(
                "argument 'min_features' is misspecified. One or more values in '",
                min_features,
                "' are not an integer value."
            ),
            call. = FALSE
        )
    }

    # The number of features requested is less than or equal to the total number of columns
    is_less_than_p <- max(min_features) <= ncol(ivs)
    if (!is_less_than_p) {
        stop(
            paste0(
                "argument 'min_features' is misspecified. You cannot ask for more features than the number of columns in the input data 'ivs'. Get rid of (", paste(min_features[min_features > ncol(ivs)], collapse = ", "), ") from the range of PCs requested."
            ),
            call. = FALSE
        )
    }
}