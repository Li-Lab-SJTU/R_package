#' Wrapper function for	imputation using Minimum value of each column.
#' @description
#' This function is a wrapper for the \code{\link{impute.MinDet}} function, which performs Min imputation on a data matrix containing missing values. Use the minimum value of each column to impute all missing values for that column
#' @param RawData  Matrix with missing values
#'
#' @return A complete matrix with missing values imputed by Minimum value of each column
#' @note  Require \code{\link{imputeLCMD}}
#' @seealso \code{\link{BPCA_fill}}, \code{\link{LLS_fill}},  \code{\link{Mean_fill}}, \code{\link{QRILC_fill}}, \code{\link{kNN_fill}}, \code{\link{SVD_fill}}, \code{\link{MLE_fill}}
#' @examples
#' ##################################### a simulated example #################
#' rm(list = ls())
#' set.seed(2020)
#' a <- matrix(data = rnorm(100000), ncol = 100) # Generate random matrix
#' a[sample(length(a), 10000)] <- NA # Generate missing values randomly
#' if(require(imputeLCMD)){
#'   a_1 <- Min_fill(a)
#' }
#'
Min_fill<-function(data){
  return(impute.MinDet(data))
}
