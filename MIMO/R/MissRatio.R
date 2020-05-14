#' Function to calculate missing ratio
#' @description Calculate the proportion of missing values in the input data. Input must be a vector, matrix, array or dataframe
#'
#' @param x a vector, matrix, array or dataframe
#'
#' @return Missing ratio of the input data
#'
#' @examples
#' ##################################### a simulated example #################
#' rm(list = ls())
#' set.seed(2020)
#' a <- matrix(data = rnorm(100000), ncol = 100) # Generate random matrix
#' a[sample(length(a), 10000)] <- NA # Generate missing values randomly
#' MissRatio(a) # Calculate the missing ratio of 'a'
#'
MissRatio <- function(x){
  if (is.numeric(x)) {
    return(sum(is.na(x))/length(x))
  }
  if (is.matrix(x) | is.data.frame(x)) {
    return(sum(is.na(x))/(nrow(x) * ncol(x)))
  }
}
