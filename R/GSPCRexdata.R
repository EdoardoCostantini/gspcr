#' GSPCR example data
#'
#' A data.frame with a dependent variable and 50 predictors generated based on true principal components.
#'
#' Data from a randomized experiment to reduce post-traumatic stress by two
#' treatments: Eye Movement Desensitization and Reprocessing (EMDR)
#' (experimental treatment), and cognitive behavioral therapy (CBT) (control
#' treatment). 52 children were randomized to one of these two treatments.
#' Outcomes were measured at three time points: at baseline (pre-treatment, T1),
#' post-treatment (T2, 4-8 weeks), and at follow-up (T3, 3 months). For more
#' details, see de Roos et al (2011).  Some person covariates were reshuffled.
#' The imputation methodology is explained in Chapter 9 of van Buuren (2012).
#'
#' @name GSPCRexdata
#' @docType data
#' @format \code{GSPCRexdata} is a data frame with 1000 rows and 51 columns:
#' \describe{
#' \item{y}{A normally distributed variable generated as the linear combination of the first two components underlying X}
#' \item{X}{A collection of predictors with 5 underlying components explaining 0.3 of the variation in X}
#' }
#' @keywords datasets
#' @examples
#'
#' data <- GSPCRexdata
#' head(GSPCRexdata)
NULL
