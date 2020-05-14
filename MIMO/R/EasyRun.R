#' Easily complete the recommendation of imputation methods
#'
#' @description  An easy-to-use way to recommend optimal imputation method
#'
#' @param RawData A two-dimensional matrix with missing values
#' @param GroupAsize Sample size of group A
#' @param GroupBsize Sample size of group B
#' @param DERatio Differential expression ratio in simulation matrix
#' @param logFC Fold Change after log2 conversion for simulation
#' @param Pair Paired experimental design or not
#' @param ExtraMethod A vector for extra methods (You are allowed to use imputation functions not included in this package, just enter their names). If there is no extra method, please enter NULL
#' @note  Require \code{\link{imputeLCMD}}
#' @return A list containing the following components:
#' \item{Total}{    Overall result}
#' \item{NRMSE}{    Top 3 with NRMSE}
#' \item{BLCI}{    Top 3 with BLCI}
#' @examples
#' ##################################### a simulated example #################
#' rm(list = ls())
#' library('imputeLCMD')
#' set.seed(2020)
#' a <- matrix(data = rnorm(100000), ncol = 100) # Generate random matrix
#' a[sample(length(a), 10000)] <- NA # Generate missing values randomly
#' rownames(a) <- seq(1, 1000, 1) # Matrix must have row names
#' res <- EasyRun(RawData = a, GroupAsize = 50, GroupBsize = 50, DEratio = 0.2, logFC = 1, Pair = T, ExtraMethod = NULL) # Evaluate input Imputation methods
#'
EasyRun <- function(RawData, GroupAsize, GroupBsize, DEratio = 0.2, logFC = 1,  Pair = TRUE, ExtraMethod = NULL)
  {
  ImputeMethod <- c(ExtraMethod, 'kNN_fill', 'SVD_fill', 'MLE_fill', 'Mean_fill', 'QRILC_fill', 'LLS_fill', 'BPCA_fill', 'Min_fill')
  RawData <- MissFilter(RawData = RawData, MaxMiss = (ncol(RawData) - 3)/ncol(RawData), byrow = T)
  MNAR<-MNARCalc(RawData = RawData, StepSize = NULL)
  res<-ImputePower(RawData = RawData, GroupAsize = GroupAsize, GroupBsize = GroupBsize,
                       DERatio = DEratio, logFC = logFC, ReTimes = 30,
                       MNARratio = MNAR[[1]][2], Pair = Pair, ImputeMethod = ImputeMethod)

  NRMSE1 <- res$NRMSE[1:3, ]
  BLCI1 <- res$BLCI[1:3, ]
  rownames(NRMSE1) <- rownames(BLCI1) <- c('1st', '2nd', '3rd')
  res <- list(res$Total, NRMSE1, BLCI1, MNAR[[1]])
  names(res) <- c('Total', 'NRMSE', 'BLCI','MNARratio')
  return(res)
}

