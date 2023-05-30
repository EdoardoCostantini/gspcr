#' GSPCR example data
#'
#' Contains a data set used to develop and test the main features of the \code{gspcr} package. The data contains a dependent variable and 50 predictors generated based on true number of principal components.
#'
#' @name GSPCRexdata
#' @docType data
#' @format \code{GSPCRexdata} is a list containing two data.frame objects:
#' - \code{X}: A list of data.frames with 1000 rows (observations) and 50 columns (possible predictors). The list contains matrices storing data coded with different measurement levels:
#'      - \code{cont} with 50 continuous variables
#'      - \code{bin} with 50 binary variables (factors)
#'      - \code{ord} with 50 ordinal variables (ordered factors)
#'      - \code{cat} with 50 categorical variables (unordered factors)
#'      - \code{mix} with 20 continuous variables, 10 binary variables (factors), 10 ordinal variables (ordered factors), 10 categorical variables (unordered factors).
#' - \code{y}: A data.frame with 1000 rows and 4 columns. The first column \code{cont} is a continuous variable produced using a linear model with the first two PCs underlying \code{X} as a data-generating model. The other columns are transformed versions of \code{cont} to match common discrete target distribution in the social sciences.
#' @keywords datasets
#' @examples
#'
#' # Check out the first 6 rows of the continuous predictors
#' head(GSPCRexdata$X$cont)
#' 
#' # Check out first 6 rows of the dv data.frame
#' head(GSPCRexdata$y)
NULL
