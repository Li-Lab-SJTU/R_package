#' Wrapper function for MLE-based imputation
#' @description
#' This function is a wrapper for the \code{\link{impute.wrapper.MLE}} function, which performs MLE imputation on a data matrix containing missing values. Use the maximum likelihood estimation principle to obtain the unknown parameter estimates obeyed by the quantitative data.
#' @param RawData  Matrix with missing values
#'
#' @return A complete matrix with missing values imputed by MLE
#' @note  Require \code{\link{imputeLCMD}}
#' @seealso \code{\link{BPCA_fill}}, \code{\link{LLS_fill}},  \code{\link{Mean_fill}}, \code{\link{QRILC_fill}}, \code{\link{kNN_fill}}, \code{\link{SVD_fill}}, \code{\link{Min_fill}}
#' @examples
#' ##################################### a simulated example #################
#' rm(list = ls())
#' set.seed(2020)
#' a <- matrix(data = rnorm(100000), ncol = 100) # Generate random matrix
#' a[sample(length(a), 10000)] <- NA # Generate missing values randomly
#' if(require(imputeLCMD)){
#'   a_1 <- MLE_fill(a)
#' }
#'

MLE_fill<-function(data){
  return(impute.wrapper.MLE(data))
}
