# Generalized Supervised Principal Component regression

An R package implementing a version of the Supervised Principal Component regression (SPCR, Bair Et. Al., 2006) that allows for any measurement level of the dependent and independent variables.
This package builds upon the method implemented in the R package [`superpc`](https://github.com/jedazard/superpc).

## Details

SPCR regresses a dependent variable onto a few supervised principal components computed from a large set of predictors.
The *steps* followed by SPCR are the following:

1. Regress the dependent variable onto each column of a data set of *p* possible predictors via *p* simple linear regressions.
2. Define a subset of the original *p* variables by discarding all variables whose univariate regression coefficient is less than a chosen threshold.
3. Use the subset of original data to estimate *q* PCs.
4. Regress the dependent variable onto the *q* PCs.

A key aspect of the method is that both the number of PCs and the threshold value can be determined by cross-validation.
GSPCR *extends* SPCR by allowing the dependent variable to be of any measurement level (i.e., ratio, interval, ordinal, nominal) by introducing likelihood-based thresholds for the univariate regressions in step 1.
Furthermore, GSPCR allows the predictors to be of any type by combining the PCAmix framework (Kiers, 1991; Chavent Et. Al., 2017) with SPCR in step 3.

## Features

The R package `gspcr` allows to:

- Estimate the GSPCR model on a training data set;
- Plot the cross-validation trends used to tune the threshold value and the number of PCs to compute;
- Predict observations on both the training data and new, previously unseen, data

## References

Bair E, Hastie T, Paul D, Tibshirani R (2006). “Prediction by supervised principal components.” J. Am. Stat. Assoc., 101(473), 119-137.

Chavent, M., Kuentz-Simonet, V., Labenne, A., & Saracco, J. (2014). Multivariate analysis of mixed data: The R package PCAmixdata. arXiv preprint arXiv:1411.4911.

Kiers, H. A. (1991). Simple structure in component analysis techniques for mixtures of qualitative and quantitative variables. Psychometrika, 56(2), 197-212.