#' Wrapper function for imputation using mean of each row
#' @description
#' This function performs mean imputation on a data matrix containing missing values. Use the average of the non-missing elements in each row.
#' @param RawData  Matrix with missing values
#'
#' @return A complete matrix with missing values imputed by mean of each row
#' @seealso \code{\link{kNN_fill}}, \code{\link{BPCA_fill}},  \code{\link{LLS_fill}}, \code{\link{QRILC_fill}}, \code{\link{SVD_fill}}, \code{\link{MLE_fill}}, \code{\link{Min_fill}}
#' @examples
#' ##################################### a simulated example #################
#' rm(list = ls())
#' set.seed(2020)
#' a <- matrix(data = rnorm(100000), ncol = 100) # Generate random matrix
#' a[sample(length(a), 10000)] <- NA # Generate missing values randomly
#' a_1 <- Mean_fill(a)
#'
Mean_fill <- function(RawData) {
  result <- RawData
  result[] <- apply(result,1,function(x) {
    x[is.na(x)] <- mean(x, na.rm = T)
    return(x)
  })
  return(result)
}
