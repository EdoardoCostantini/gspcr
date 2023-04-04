% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LL_gaussian.R
\name{LL_gaussian}
\alias{LL_gaussian}
\title{Gaussian log-likelihood}
\usage{
LL_gaussian(y, y_hat, mod)
}
\arguments{
\item{y}{Vector of observed values on a continuous dependent variable.}

\item{y_hat}{Vector of predicted values by the model.}

\item{mod}{\code{glm} or \code{lm} object containing and estimated linear regression model.}
}
\value{
Atomic vector of length 1 containing the log-likelihood value.
}
\description{
Computes the gaussian (normal) log-likelihood of a vector of observed values given a trained linear regression model.
}
\author{
Edoardo Costantini, 2022
}