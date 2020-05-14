#' Function for missing filtering
#' @description Filter out rows/columns where the number of missing values is more than the input value
#'
#' @param RawData A matrix that requires missing filtering
#' @param MaxMiss Maximum percentage of missing values allowed per row/columns
#' @param byrow Whether to filter by rows. If not, proceed by column
#'
#' @return Matrix after missing filtering
#'
#' @examples
#' ##################################### a simulated example #################
#' rm(list = ls())
#' set.seed(2020)
#' a <- matrix(data = rnorm(100000), ncol = 100) # Generate random matrix
#' a[sample(length(a), 10000)] <- NA # Generate missing values randomly
#' a_1 <- MissFilter(RawData = a, MaxMiss = 0.1, byrow = T) # Filter out rows with more than 50% missing values
#'
MissFilter <- function (RawData, MaxMiss = 0.5, byrow = TRUE ){
  m <- ifelse(byrow, 1, 2)
  l <- switch(m, ncol(RawData), nrow(RawData))
  sav <- which(apply(RawData, m, function(x){ sum(is.na(x)) }) <= MaxMiss * l)
  if (length(sav) > 0) { return(RawData[sav,]) }
  else {return(NULL)}
}
