#' Wrapper function for KNN imputation
#' @description
#' This function is a wrapper for the \code{\link{impute.knn}} function, which performs KNN imputation on a data matrix containing missing values. Impute missing values by weighted average abundance of k most similar peptides
#' @param RawData  Matrix with missing values
#' @param K The number of neighbors used to infer the missing data.
#'
#' @return A complete matrix with missing values imputed by kNN
#' @note  Require \code{\link{imputeLCMD}}
#' @seealso \code{\link{BPCA_fill}}, \code{\link{LLS_fill}},  \code{\link{Mean_fill}}, \code{\link{QRILC_fill}}, \code{\link{SVD_fill}}, \code{\link{MLE_fill}}, \code{\link{Min_fill}}
#' @examples
#' ##################################### a simulated example #################
#' rm(list = ls())
#' set.seed(2020)
#' a <- matrix(data = rnorm(100000), ncol = 100) # Generate random matrix
#' a[sample(length(a), 10000)] <- NA # Generate missing values randomly
#' if(require(imputeLCMD)){
#'   a_1 <- kNN_fill(a)
#' }
#'
kNN_fill <- function(RawData, K = 10) {
  RawData <- as.matrix(RawData)
  kNNres <- impute.knn(RawData, k = K, colmax = 0.99, rowmax = 0.99)
  return(kNNres[[1]])
}

