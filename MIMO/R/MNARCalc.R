#' Calculate MNAR ratio of input data
#' @description
#' The MNAR ratio of the input matrix is estimated by establishing simulation matrices. Generate corresponding simulation matrices based on the missing ratio of the input data and each MNAR ratio in the given range. Then calculate the maximum vertical difference between the distributions (D.value) of each simulation matrix and the input matrix. The MNAR ratio with the smallest D.value is the estimation result.
#' @param RawData A two-dimensional matrix with missing values
#' @param StepSize The percentage of growth for each step (from 0\% to 100\% ). The smaller the more accurate but more time consuming. You can enter NULL and we will automatically calculate the step size based on the matrix size
#'
#' @return A list containing the following components:
#' \item{MNARratio}{ MNAR ratio of the input matrix}
#' \item{D.res}{ D.value corresponding to each MNAR}
#'
#' @examples
#' ##################################### a simulated example #################
#' rm(list = ls())
#' set.seed(2020)
#' a <- matrix(data = rnorm(100000), ncol = 100) # Generate random matrix
#' a[sample(length(a), 10000)] <- NA # Generate missing values randomly
#' res <- MNARCalc(RawData = a, StepSize = NULL) # Calculating MNAR ratio
#'
#'
MNARCalc <- function(RawData, StepSize = NULL){
  if (is.null(StepSize)) {
    l <- nrow(RawData) * ncol(RawData)
    StepSize <- round(0.005 * sqrt(l/10^6), digits = 3)
    if (StepSize > 0.03) {
      StepSize <- 0.03
    }
    if (StepSize < 0.001) {
      StepSize <- 0.001
    }
  }
  MNARrange <- round(x = seq(0, 1, StepSize), digits = 3)
  RawData <- as.matrix(RawData)
  MisRatio <- sum(is.na(RawData)) / (nrow(RawData) * ncol(RawData))
  D.value <- c()
  SimData <- t(apply(RawData, 1, function(x){
    rnorm(ncol(RawData), mean = mean(x, na.rm=T), sd = sd(x, na.rm = T))
  }))
  rownames(SimData) <- rownames(RawData)
  logistic <- function(x){ exp(x)/(1 + exp(x)) }
  p.marr <- 1 - logistic((-mean(SimData, na.rm=T) + SimData))
  p.mean <- apply(p.marr, 2, mean,na.rm = T)
  for (i in MNARrange) {
    MisData <- SimData
    p_1 <- c()
    for (a in 1:ncol(p.marr)) {
      p.cmis <- sample(1:nrow(p.marr), round(p.mean[a]/sum(p.mean) * length(p.marr) * MisRatio * i), prob =  p.marr[,a])
      p_1 <- c(p_1, ((a - 1) * nrow(p.marr) + p.cmis))
    }
    p_2 <- sample(setdiff(seq(1, length(p.marr), 1), p_1), round(length(p.marr) * MisRatio * (1-i)))
    MisData[c(p_1, p_2)] <- NA
    raw <- RawData[!is.na(RawData)]
    mis <- MisData[!is.na(MisData)]
    z <- cumsum(ifelse(order(c(raw, mis)) <= length(raw), 1/length(raw), -1/length(mis)))
    D.value <- c(D.value, max(abs(z)))
  }
  res <- cbind(D.value, MNARrange)
  res <- list(res[order(res[, 1]),][1,], res)
  names(res[[1]]) <- c('D.value', 'MNARratio')
  names(res) <- c('MNARratio', 'D.res')
  return(res)
}

