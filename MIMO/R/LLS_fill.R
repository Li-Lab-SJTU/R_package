#' Wrapper function for LLS-based imputation
#' @description
#' This function is a wrapper for the \code{\link{llsImpute}} function, which performs LLS imputation on a data matrix containing missing values. Impute missing values based on multivariate least squares estimation of K nearest rows.
#' @param RawData  Matrix with missing values
#'
#' @return A complete matrix with missing values imputed by LLS
#' @note  Require \code{\link{pcaMethods}}
#' @seealso \code{\link{kNN_fill}}, \code{\link{BPCA_fill}},  \code{\link{Mean_fill}}, \code{\link{QRILC_fill}}, \code{\link{SVD_fill}}, \code{\link{MLE_fill}}, \code{\link{Min_fill}}
#' @examples
#' ##################################### a simulated example #################
#' rm(list = ls())
#' set.seed(2020)
#' a <- matrix(data = rnorm(100000), ncol = 100) # Generate random matrix
#' a[sample(length(a), 10000)] <- NA # Generate missing values randomly
#' if(require(pcaMethods)){
#'   a_1 <- LLS_fill(a)
#' }
#'
LLS_fill <- function(RawData) {
  result <- llsImpute(RawData,k = 5)
  return (result@completeObs)
}
