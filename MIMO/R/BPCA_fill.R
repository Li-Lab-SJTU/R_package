#' Wrapper function for BPCA-based imputation
#' @description
#' This function is a wrapper for the \code{\link{pca}} function, which performs BPCA imputation on a data matrix containing missing values. Use the variational Bayes algorithm to estimate the posterior distribution of the model parameters.
#' @param RawData  Matrix with missing values
#'
#' @return A complete matrix with missing values imputed by BPCA
#' @note  Require \code{\link{pcaMethods}}
#' @seealso \code{\link{kNN_fill}}, \code{\link{LLS_fill}},  \code{\link{Mean_fill}}, \code{\link{QRILC_fill}}, \code{\link{SVD_fill}}, \code{\link{MLE_fill}}, \code{\link{Min_fill}}
#' @examples
#' ##################################### a simulated example #################
#' rm(list = ls())
#' set.seed(2020)
#' a <- matrix(data = rnorm(100000), ncol = 100) # Generate random matrix
#' a[sample(length(a), 10000)] <- NA # Generate missing values randomly
#' if(require(pcaMethods)){
#'   a_1 <- BPCA_fill(a)
#' }
#'
#'
BPCA_fill <- function(RawData) {
  result <- pca(RawData, method="bpca", nPcs=2)
  return (result@completeObs)
}
