#' Wrapper function for SVD-based imputation
#' @description
#' This function is a wrapper for the \code{\link{impute.wrapper.SVD}} function, which performs SVD imputation on a data matrix containing missing values. Use the k ranks of the matrix to continuously iterate to convergence.
#' @param RawData  Matrix with missing values
#' @param K The number of PCs used.
#'
#' @return A complete matrix with missing values imputed by SVD
#' @note  Require \code{\link{imputeLCMD}}
#' @seealso \code{\link{BPCA_fill}}, \code{\link{LLS_fill}},  \code{\link{Mean_fill}}, \code{\link{QRILC_fill}}, \code{\link{kNN_fill}}, \code{\link{MLE_fill}}, \code{\link{Min_fill}}
#' @examples
#' ##################################### a simulated example #################
#' rm(list = ls())
#' set.seed(2020)
#' a <- matrix(data = rnorm(100000), ncol = 100) # Generate random matrix
#' a[sample(length(a), 10000)] <- NA # Generate missing values randomly
#' if(require(imputeLCMD)){
#'   a_1 <- SVD_fill(a)
#' }
#'

SVD_fill <- function(RawData,K=2){
  mean_sd<-data.frame(mean<-apply(RawData,1,function(x){mean(x, na.rm=T)}),sd<-apply(RawData,1,function(x){sd(x, na.rm=T)}))
  res_sca<-apply(RawData,1,scale)
  res_SVD<-impute.wrapper.SVD(res_sca,K=K)
  result<-(t(res_SVD)*mean_sd[,2]+mean_sd[,1])
  return(result)
}
