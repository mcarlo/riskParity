Assets <- c("Bonds", "Large Cap", "Small Cap", "Developed Non-US", "Emerging Market", "Commodities")

weights <- c(0.10, 0.4, 0.08, 0.12, 0.06, 0.24)

vols <- c(0.07, 0.20, 0.25, 0.22, 0.30, 0.25)
correls <- matrix(rep(0.0, 36), nrow = 6)

correls[lower.tri(correls)] <- runif(15)
correls[upper.tri(correls)] <- t(correls)[upper.tri(t(correls))]
diag(correls) <- 1

covMatrix <- correls * (vols %*% t(vols))

contribVariance <- function(weightVec, covMtrx){
  weightVec * (covMtrx %*% weightVec)
}

swapRatios <- function(weightVec, covMtrx){
  ctv <- contribVariance(weightVec, covMtrx)
  (ctv - mean(ctv))/2
}

calibrateWeights <- function(weightVec, covMtrx){
  portWeights <- weightVec
  swap <- swapRatios(portWeights, covMtrx)
  portWeights - swap
}

riskParity <- function(covMtrx, epsilon = .001, keepCount = F){#covMtrx = covMatrix
  count <- 0
  if (max(covMtrx) > 1) covMtrx <- (covMtrx/10)/max(covMtrx)
  nAssets <- dim(covMtrx)[1]
  #newWeights <- rep(1/nAssets, nAssets)
  newWeights <- sum(diag(covMtrx))/diag(covMtrx)
  ctv <- contribVariance(newWeights, covMtrx)
  dispersion <- (max(ctv) - min(ctv))/max(ctv)

  while (dispersion > epsilon){
    count <- count + 1
    newWeights <- calibrateWeights(newWeights, covMtrx)
    ctv <- contribVariance(newWeights, covMtrx)
    dispersion <- (max(ctv) - min(ctv))/max(ctv)
  }
  if (keepCount) print(paste0(count, " iterations"))
  newWeights/sum(newWeights)
}