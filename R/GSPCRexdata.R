#' GSPCR example data
#'
#' A data.frame with a dependent variable and 50 predictors generated based on true principal components.
#'
#' @name GSPCRexdata
#' @docType data
#' @format \code{GSPCRexdata} is a list containing two data.frame objects:
#' \describe{
#' \item{X}{A data.frame with 1000 rows and 50 columns of possible predictors. These predictors were generated such that 30% of their total variability could be explained by 5 principal components.}
#' \item{y}{A data.frame with 1000 rows and 4 columns. The first column \code{cont} is a continuous variable produced using a linear model with the first two PCs underlying \code{X} as a data-generating model. The other columns are transformed versions of \code{cont} to match common discrete target distribution in the social sciences.}
#' }
#' @keywords datasets
#' @examples
#'
#' data <- GSPCRexdata
#' head(GSPCRexdata)
NULL
