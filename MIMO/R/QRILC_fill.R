#' Wrapper function for imputation using QRILC method.
#' @description
#' This function is a wrapper for the \code{\link{impute.QRILC}} function, which performs QRILC imputation on a data matrix containing missing values. Missing value filling by randomly extracting truncated distributions from quantile regression estimates
#' @param RawData  Matrix with missing values
#'
#' @return A complete matrix with missing values imputed by QRILC imputation
#' @note  Require \code{\link{imputeLCMD}}
#' @seealso \code{\link{kNN_fill}}, \code{\link{BPCA_fill}},  \code{\link{Mean_fill}}, \code{\link{LLS_fill}}, \code{\link{SVD_fill}}, \code{\link{MLE_fill}}, \code{\link{Min_fill}}
#' @examples
#' ##################################### a simulated example #################
#' rm(list = ls())
#' set.seed(2020)
#' a <- matrix(data = rnorm(100000), ncol = 100) # Generate random matrix
#' a[sample(length(a), 10000)] <- NA # Generate missing values randomly
#' if(require(imputeLCMD)){
#'   a_1 <- QRILC_fill(a)
#' }
#'
QRILC_fill <- function(RawData) {
  data <- as.data.frame(RawData)
  result <- impute.QRILC(RawData)[[1]]
  return(as.matrix(result))
}
