% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LL_newdata.R
\name{LL_newdata}
\alias{LL_newdata}
\title{Log-Likelihood for new data}
\usage{
LL_newdata(y_train, y_valid, X_train, X_valid, fam)
}
\arguments{
\item{y_train}{Vector of DV values in the training dataset.}

\item{y_valid}{Vector of DV values in the validation dataset.}

\item{X_train}{Matrix of IV values in the training dataset. Can also be set to 1 to obtain the log-likelihood of the new data under the null model.}

\item{X_valid}{Matrix of IV values in the validation dataset. If \code{X_train} is set to 1 to obtain the log-likelihood of the new data under the null model, \code{X_valid} is ignored.}

\item{fam}{GLM framework for the dv.}
}
\value{
A list of objects.
}
\description{
Given training and validation datasets, this function returns the log-likelihood of unobserved data under the model trained on the training data.
}
\details{
This function trains a GLM regressing \code{y_train} on \code{X_train} using as link function what is specified in \code{fam}. Then, it computes the predictions for the validation data based on the trained model on the scale of the linear predictors (e.g., logit). The likelihood of the validation under the model is returned.
}
\references{
Such, S. (2006). Such and such. Journal such and such, 101(473), 119-137.
}
\author{
Edoardo Costantini, 2023
}
