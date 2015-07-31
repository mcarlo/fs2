library(combinat)
testM <- 1
x1 <- matrix(unlist(combn(1:16, m = testM)), nrow = testM) # 16

testM <- 2
x2 <- matrix(unlist(combn(1:16, m = testM)), nrow = testM) # 120

testM <- 3
x3 <- matrix(unlist(combn(1:16, m = testM)), nrow = testM) # 560

testM <- 4
x4 <- matrix(unlist(combn(1:16, m = testM)), nrow = testM) # 1820

# 1 + 16 + 120 + 560 + 1820 = 2517

testM <- 5
x5 <- matrix(unlist(combn(1:16, m = testM)), nrow = testM) # 4368

testM <- 6
x6 <- matrix(unlist(combn(1:16, m = testM)), nrow = testM) # 8008

testM <- 7
x7 <- matrix(unlist(combn(1:16, m = testM)), nrow = testM) # 11440

testM <- 8
x8 <- matrix(unlist(combn(1:16, m = testM)), nrow = testM) # 12870

library(foreach)
createTwinPicks <- function(xMatrix){
  nCols <- dim(xMatrix)[2]
  outMat <- matrix(rep(1, 16 * nCols), ncol = nCols)
  foreach(j = 1:nCols, .combine = cbind) %do% {outMat[xMatrix[, j], j] <- 0}
  outMat
}

choose(16, 2)

populateTwinPicks <- function(xMatrix){ #xMatrix <- x8
  nCols <- dim(xMatrix)[2]
  chooseN <- dim(xMatrix)[1]
  if(chooseN > 1) {
    startCol <- 2 + foreach(n = 1:(chooseN - 1), .combine = '+') %do% {choose(16, n)}
  } else {
    startCol <- 2
  }
  endCol <- startCol + choose(16, chooseN) - 1
  plugMatrix <- matrix(rep(1, 16 * nCols), nrow = 16)
  for(j in 1:nCols){
    plugMatrix[xMatrix[, j], j] <- 0
  }
  comparisonPicks[, startCol:endCol] <<- plugMatrix
}

comparisonPicks <- matrix(rep(1, 16 * (39203 * 2 + 1)), nrow = 16)

weekFile <- read.csv("2014week17.csv")
winprob <- weekFile$WinProbability
outcomeMatrix <- matrix(runif(32000) < winprob, nrow = 16)

comparisonPicksScores <- crossprod(outcomeMatrix, comparisonPicks) + crossprod((1- outcomeMatrix), (1 - comparisonPicks))

fanprob <- weekFile$FanProb
fanMatrix <- matrix(runif(32000) < fanprob, nrow = 16)
fanScores <- crossprod(outcomeMatrix, fanMatrix) + crossprod((1- outcomeMatrix), (1 - fanMatrix))

fanScoreSubset <- matrix(rep(0, 200000), nrow = 2000)

poolsize <- 50
sampleFans <- matrix(sample(1:2000, 2000 * poolsize, replace = T), nrow = 2000)
for (i in 1:2000){
  fanScoreSubset[i, ] <- fanScores[i, sampleFans[i, ]]
}

comparisonFirst <- comparisonPicksScores > apply(fanScoreSubset, 1, max)
comparisonTied <- comparisonPicksScores >= apply(fanScoreSubset, 1, max)
outright <- which(colSums(comparisonFirst) == max(colSums(comparisonFirst)))
mostwins <- which(colSums(comparisonTied) == max(colSums(comparisonTied)))

comparisonPicks[, outright]
comparisonPicks[, mostwins]

colSums(comparisonFirst)[outright]
colSums(comparisonTied)[mostwins]
