#' Estimate the power of imputation methods based on input data
#' @description
#' By establishing simulation matrices, the effectiveness of each Imputation method is evaluated from the two levels of NRMSE and BLCI (TPR + TNR - 1). First generate the corresponding simulation matrix based on the input matrix and dataset features. Then after generating the missing values, use each Imputation method to be evaluated to impute the missing values. Compare the initial simulation matrix with the filled simulation matrix to get the result. Repeat to eliminate random errors
#'
#' @param RawData A two-dimensional matrix with missing values
#' @param GroupAsize Sample size of group A
#' @param GroupBsize Sample size of group B
#' @param DERatio Differential expression ratio in simulation matrix
#' @param logFC Fold Change after log2 conversion for simulation
#' @param ReTimes Repeat times. To eliminate random error. The smaller the more accurate but more time consuming. You can enter NULL and we will automatically calculate the repeat times based on the matrix size
#' @param Pair Paired experimental design or not
#' @param MNARratio MNAR ratio of the input matrix
#' @param ImputeMethod Name of imputation methods for evaluation and comparison (You can use imputation functions not included in this package,just enter the name)
#' @note  Require \code{\link{imputeLCMD}}
#' @return
#' @return A list containing the following components:
#' \item{Total}{    Overall result}
#' \item{NRMSE}{    NRMSE of each imputation method}
#' \item{BLCI}{    BLCI of each imputation method}
#' @examples
#' ##################################### a simulated example ###################
#' rm(list = ls())
#' library('imputeLCMD')
#' set.seed(2020)
#' a <- matrix(data = rnorm(100000), ncol = 100) # Generate random matrix
#' a[sample(length(a), 10000)] <- NA # Generate missing values randomly
#' rownames(a) <- seq(1, 1000, 1) # Matrix must have row names
#' res <- ImputePower(RawData = a, GroupAsize = 50, GroupBsize = 50, DERatio = 0.2, logFC = 1, ReTimes = 30, Pair = T, MNARratio = 0,
#'                    ImputeMethod = c('kNN_fill', 'SVD_fill', 'MLE_fill', 'Mean_fill', 'QRILC_fill', 'LLS_fill', 'BPCA_fill', 'Min_fill')) # Evaluate input Imputation methods
#'
#'
#'

ImputePower <- function(RawData, GroupAsize, GroupBsize, DERatio = 0.2, logFC = 1, ReTimes = NULL, Pair, MNARratio,
                            ImputeMethod = c('kNN_fill', 'SVD_fill', 'MLE_fill', 'Mean_fill', 'QRILC_fill',
                                             'LLS_fill', 'BPCA_fill', 'Min_fill'))
{
  TtestDE <- function(dataset){
    group_list <- as.character(c(rep("A", GroupAsize), rep("B", GroupBsize)))
    if (Pair) {
      res <- t(apply(dataset, 1, function(x){
        pvfc <- t.test(x ~ group_list, paired=T, var.equal = T)
        return(c(pvfc$p.value, pvfc$estimate))
      }))
    }
    else{
      res <- t(apply(dataset, 1, function(x){
        pvfc <- t.test(x ~ group_list, paired=F, var.equal = T)
        return(c(pvfc$p.value, pvfc$estimate[1] - pvfc$estimate[2]))
      }))
    }
    DEset <- c()
    res[,1] <- p.adjust(res[,1], method = 'BH')
    for (i in 1:nrow(res)) {
      if (res[i, 1] < 0.05 & res[i, 2] < 0-logFC) {
        DEset <- c(DEset, rownames(dataset)[i])
      }
    }
    return(DEset)
  }
  if (is.null(ReTimes)) {
    l <- nrow(RawData) * ncol(RawData)
    ReTimes <- ceiling(20 * sqrt(10^6/l))
    if (ReTimes > 100) {
      ReTimes <- 100
    }
    if (ReTimes < 10) {
      ReTimes <- 10
    }
  }
  RawData <- as.matrix(RawData)
  MisRatio <- sum(is.na(RawData)) / (nrow(RawData) * ncol(RawData))
  res <- matrix(ncol = (2 * length(ImputeMethod)), nrow = ReTimes)
  for (k in 1:ReTimes) {
    SimData <- t(apply(RawData, 1, function(x){
      rnorm((GroupAsize + GroupBsize), mean = mean(x, na.rm = T), sd = sd(x, na.rm = T))
    }))
    rownames(SimData) <- rownames(RawData)
    FCadd <- sample(nrow(SimData), round(DERatio * nrow(SimData)))
    SimData[FCadd, (GroupAsize + 1):(GroupAsize + GroupBsize)] <- logFC + SimData[FCadd, (GroupAsize + 1):(GroupAsize + GroupBsize)]
    Gold <- TtestDE(dataset = SimData)
    logistic <- function(x){ exp(x)/(1 + exp(x)) }
    p.marr <- 1 - logistic((-mean(SimData, na.rm=T) + SimData))
    p.mean <- apply(p.marr, 2, mean,na.rm = T)
    p_1 <- c()
    for (a in 1:ncol(p.marr)) {
     p.cmis <- sample(1:nrow(p.marr), round(p.mean[a]/sum(p.mean) * length(p.marr) * MisRatio * MNARratio), prob =  p.marr[,a])
     p_1 <- c(p_1, ((a - 1) * nrow(p.marr) + p.cmis))
    }
    p_2 <- sample(setdiff(1:length(p.marr), p_1), round(length(p.marr) * MisRatio * (1 - MNARratio)))
    MisData <- SimData
    MisData[c(p_1, p_2)] <- NA
    r <- c()
    for (i in ImputeMethod) {
      ImpData <- do.call(i, list(MisData))
      Imp <-  TtestDE(dataset = ImpData)
      misseq <- is.na(MisData)
      NRMSE <- sqrt(mean((ImpData[misseq] - SimData[misseq])^2)/var(SimData[misseq]))
      BLCI <- length(intersect(Gold, Imp))/length(Gold) - (length(Imp) - length(intersect(Gold, Imp)))/(nrow(SimData) - length(Gold))
      r <- c(r, NRMSE, BLCI)
    }
    res[k,] <- r
  }
  res <- data.frame(matrix(apply(res, 2, mean, na.rm=T), ncol=2, byrow = T), ImputeMethod)
  colnames(res) <- c("NRMSE", "BLCI", "Method")
  NRMSE1 <- res[order(res[, 1]),][, c(1, 3)]
  BLCI1 <- res[order(res[, 2],decreasing = T),][, c(2, 3)]
  rownames(res) <- ImputeMethod
  rownames(NRMSE1) <- rownames(BLCI1) <- seq(1, length(ImputeMethod), 1)
  res <- list(res[,1:2], NRMSE1, BLCI1)
  names(res) <- c('Total', 'NRMSE', 'BLCI')
  return(res)
}
