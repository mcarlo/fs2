processFile <- function(weekFilename){
  #rm(list = ls())
  #load("fansimsSkeleton.RData")
  #weekFilename = "2014week16.csv"
  # these objects will vary by week
  #

  #setwd("~/GitHub/fs2")
  weekFile <- read.csv(weekFilename, stringsAsFactors = F)# "2014week01straight.csv", stringsAsFactors = F)


  # weekFile <<- read.csv(weekFilename, stringsAsFactors = F, colClasses = c("character", "numeric", "integer",
                                                                          #"numeric", "numeric", "numeric", "numeric",
                                                                          #"character"))
  games <<- length(weekFile$Confidence)
  gameRanks <<- games:1

  weekFileConf <<- weekFile[order(-weekFile$Confidence),]


#   if (any(rank(weekFile$Confidence) != games:1)){
#     weekFile <<- weekFile[order(-weekFile$Confidence),]
#   }

  winProb <<- weekFileConf$WinProbability

  if (max(winProb) > 1) {winProb <<- winProb/100.0}

  favorites <<- weekFileConf$Victor
  strategies <<- matrix(rep(favorites, 14), ncol = 14)

  # simulate whether fans pick the favorite
  fanProb <<- weekFileConf$FanProb

  # simulate favorite confidence and underdog confidence
  favConf <<- weekFileConf$FavConf
  dogConf <<- weekFileConf$DogConf

  dogs <<- weekFileConf$Underdog

#   oppLabel <- function(c){paste0(c, "'s opponent")}
#   dogs <<- sapply(favorites, oppLabel)
#   if(dim(weekFile)[2] == 8) {dogs <<- weekFile$Underdog}

#   suppressMessages(suppressWarnings(library(data.table)))
#
#   weekFileDT <<- data.table(weekFile, key = Victor)

  #save.image("procFile.RData")
  ### dependent matrices
}

assignPoolPoints <- function(numFans){
  poolPoints <<- totalPointsIter[, 1:numFans]
}

calcPoints <- function(rankVec, outcomeMatrix = simOutcomes2){
  pointVec <- as.vector(t(crossprod(rankVec, outcomeMatrix)))
}

conditionGames <- function(nGames){
  if (nGames == 15){
    simDogs <<- simDogs15
    simFavs <<- simFavs15
    simOutcomes2 <<- simOutcomes2_15
    simPicks <<- simPicks15
    simplayerCols <<- simplayerCols15
    simPrior <<- simPrior15
    simRand <<- simRand15
    simRaw <<- simRaw15
    upsetMatrix <<- upsetMatrix15
    upsetDiagMatrix <<- upsetDiagMatrix15
    fanIndex <<- fanIndex15
  } else if (nGames == 14){
    simDogs <<- simDogs14
    simFavs <<- simFavs14
    simOutcomes2 <<- simOutcomes2_14
    simPicks <<- simPicks14
    simplayerCols <<- simplayerCols14
    simPrior <<- simPrior14
    simRand <<- simRand14
    simRaw <<- simRaw14
    upsetMatrix <<- upsetMatrix14
    upsetDiagMatrix <<- upsetDiagMatrix14
    fanIndex <<- fanIndex14
  } else if(nGames == 13){
    simDogs <<- simDogs13
    simFavs <<- simFavs13
    simOutcomes2 <<- simOutcomes2_13
    simPicks <<- simPicks13
    simplayerCols <<- simplayerCols13
    simPrior <<- simPrior13
    simRand <<- simRand13
    simRaw <<- simRaw13
    upsetMatrix <<- upsetMatrix13
    upsetDiagMatrix <<- upsetDiagMatrix13
    fanIndex <<- fanIndex13
  } 
}

genMtx <- function(){
  #rm(list = ls())
  #load("procFile.RData")
  #games <<- length(winProb)
  premium <<- 16 - games
  prem <<- FALSE
  premiumPts <<- 0 + prem * premium

  selectRows <<- (1:games)
  selectRowsPrem <<- selectRows + (1 - prem) * premium
  for (j in 2:14){ # j = 2
    strategies[j - 1, j] <<- dogs[j-1] #weekFile[1:3, ]; favorites[j]
    strategies[, j] <<- strategies[order(-(upsetMatrix[selectRows, selectRows] + upsetDiagMatrix[selectRows, selectRows])[ , j - 1]), j]
  }
  #strategies <<- strategies

  simPicks <<- matrix((simplayerCols[selectRows, ]  < fanProb)*1, nrow = games, ncol = playerCols)

  simFavs <<- matrix(qbinom(simRand[1:games], games, (favConf - .5)/games, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
  # simFavs[1:10, 1:10]

  simDogs <<- matrix(qbinom(simRand[1:games], games, (dogConf - .5)/games, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
  simPrior <<- matrix(qbinom(simRand[1:games], games, 0.5, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
  #rm(simplayerCols); rm(simRand)

  simRaw <<- (simPrior + simFavs *simPicks + simDogs *(1 - simPicks))/2

  simRanks <<- apply(simRaw[selectRows,], 2, rank) + premiumPts # max(apply(simRanks, 2, max))
  #rm(simRaw); rm(simPrior); rm(simFavs); rm(simDogs)

  simOutcomes2 <<- (simOutcomes2[selectRows,] <= winProb) * 1

  totalPoints <<- t(crossprod((simPicks * simRanks), simOutcomes2) +
                      crossprod((1 - simPicks) * simRanks, (1 - simOutcomes2)))

  myRanks <<- rank(winProb, ties.method = "random")+premiumPts
  myPoints <<- calcPoints(myRanks)

  upsetPoints <<- t(crossprod(upsetMatrix[selectRows,selectRows,  drop = F], simOutcomes2) +
                      crossprod(upsetDiagMatrix[selectRows,selectRows,  drop = F], (1 - simOutcomes2)))


  altUpsetPoints <<- t(crossprod(altUpsets[selectRows,selectRows,  drop = F], simOutcomes2))

  altUpsetPoints2 <<- t(crossprod(altUpsets2[selectRows,selectRows,  drop = F], simOutcomes2) +
                      crossprod(altUpsetsRow[selectRows,selectRows,  drop = F], (1 - simOutcomes2)))

  stratMatrix <<- matrix(cbind(myPoints[resultIndex], upsetPoints[resultIndex,]), nrow = 2000)

  altStratMatrix <<- matrix(cbind(altUpsetPoints[resultIndex,], altUpsetPoints2[resultIndex,]), nrow = 2000)

}

# cmpMtx <- cmpfun(genMtx)


simParams <- function(){
  suppressMessages(require(foreach))
  maxIter <<- 2000
  set.seed(123)
  # resultIndex <<- sample(1:2000, maxiter, replace = TRUE)
  fanIndex <<- matrix(as.numeric(foreach(resultIndex, .combine = rbind) %do% as.numeric(sample(1:playerCols, 250, replace = T))), nrow = 2000)
  rowMax <<- 2000
  stratWins <<- rep(0, 14)
  stratPlace <<- rep(0, 14)
  stratShow <<- rep(0, 14)

#   myRanks <<- rank(winProb, ties.method = "random")+premiumPts
#   myPoints <<- calcPoints(myRanks)
#
#   upsetPointsMatrix <<- apply(upsetPoints, 2, calcPoints) + apply(upsetPoints, 2, calcPoints, outcomeMatrix = 1 - simOutcomes2)

#   stratMatrix <<- matrix(cbind(myPoints[resultIndex], upsetPointsMatrix[resultIndex,]), nrow = 2000)

}

rankVinM_Q <- function(vec, pointsMtrx){
  temp <- -matrix(cbind(vec, pointsMtrx), ncol = dim(pointsMtrx)[2] + 1)
  rankM <- t(apply(temp, 1, rank, ties.method = "min"))[, 1]
  rankM
}

littleSim <- function() { #, totalPointsMatrix = totalPointsIter,
                      #upsetPointsMatrix = upsetPoints){# myPointsVector = myPoints){ #numFans = 25

  totalPointsIter <<- matrix(foreach(i = 1:rowMax, .combine = rbind) %do%
                               # i = 1
                               totalPoints[resultIndex[i], fanIndex[i,]], nrow = rowMax, ncol = 250)
  #totalPointsIter
} #littleSim(250)

calcWinners <- function(numberFans = numFans){

  totalPointsMatrix <<- totalPointsIter[, 1:numberFans] #totalPointsMatrix[1:10,]

  rankMatrix <<- apply(stratMatrix, 2, rankVinM_Q, pointsMtrx = totalPointsMatrix)
  altRankMatrix <<- apply(altStratMatrix, 2, rankVinM_Q, pointsMtrx = totalPointsMatrix)


  stratWins <<- colSums(rankMatrix[, 1:14] == 1)
  stratPlace <<- colSums(rankMatrix[, 1:14] == 2)
  stratShow <<- colSums(rankMatrix[, 1:14] == 3)

  altStratWins <<- colSums(altRankMatrix == 1)
  altStratPlace <<- colSums(altRankMatrix == 2)
  altStratShow <<- colSums(altRankMatrix == 3)


  resultsMatrix <<- as.matrix(cbind(stratWins, stratPlace, stratShow), nrow = 6, ncol = 3) * 17.0 / maxIter
  altResultsMatrix <<- as.matrix(cbind(altStratWins, altStratPlace, altStratShow), nrow = 6, ncol = 3) * 17.0 / maxIter

  resultsMatrix
}

computeWinnings <- function(resultsMatrix, payouts = c(100, 0, 0)) {
  winnings <- round(as.data.frame(t((resultsMatrix %*% payouts))), 1)
  colnames(winnings) <- c("WTP", "Fav", "Fav-1", "Fav-2", "Fav-3", "Fav-4",
                           "Fav-5", "Fav-6", "Fav-7", "Fav-8", "Fav-9",
                           "Fav-10", "Fav-11", "Fav-12")
  rownames(resultsMatrix) <- colnames(winnings)
  winnings
}

computeAltWinnings <- function(altResultsMatrix, payouts = c(100, 0, 0)) {
  altWinnings <- round(as.data.frame(t((altResultsMatrix %*% payouts))), 1)
  altWinnings
}

countITM <- function(resultsMatrix, payouts = c(100, 0, 0)) {
  inTheMoney <- round(rowSums(resultsMatrix %*% (1*(payouts > 0))), 2)
  inTheMoney
}

simulatePool <- function(numFans = 100,
                         payouts = c(100, 0, 0), totalPointsMatrix = totalPointsIter[, 1:numFans],
                         myPointsVector = myPoints, upsetPointsMatrix = upsetPoints){

  myRanks <<- rank(winProb, ties.method = "random")+premiumPts

  myPoints <<- as.vector(crossprod(myRanks, simOutcomes2)) # * myRanks

  stratMatrix <- matrix(cbind(myPoints[resultIndex], upsetPointsMatrix[resultIndex,]), nrow = 2000)

  rankMatrix <- apply(stratMatrix, 2, rankVinM_Q, pointsMtrx = totalPointsMatrix)
  stratWins <- colSums(rankMatrix[, 1:14] == 1)
  stratPlace <- colSums(rankMatrix[, 1:14] == 2)
  stratShow <- colSums(rankMatrix[, 1:14] == 3)

  resultsMatrix <<- as.matrix(cbind(stratWins, stratPlace, stratShow), nrow = 6, ncol = 3) * 17.0 / maxIter
  winnings <<- round(as.data.frame(t((resultsMatrix %*% payouts))), 1)
  inTheMoney <<- round(rowSums(resultsMatrix %*% (1*(payouts > 0))), 2)

  colnames(winnings) <<- c("WTP", "Fav", "Fav-1", "Fav-2", "Fav-3", "Fav-4",
                           "Fav-5", "Fav-6", "Fav-7", "Fav-8", "Fav-9",
                           "Fav-10", "Fav-11", "Fav-12")
  rownames(resultsMatrix) <<- colnames(winnings)
#   top3Money()
#   top3Dollars()
  topWin <<- order(-winnings[1,])[1:3]
  topMoney <<- order(-inTheMoney)[1:3]

  #print(resultsMatrix)
  #   print(rbind(round(winnings, 2), round(apply(resultsMatrix, 1, sum), 1)))

}

simulateOld <- function(maxIter = 2000, numFans = 90,
                         payouts = c(100, 0, 0), totalPointsMatrix = totalPointsIter,
                         myPointsVector = myPoints, upsetPointsMatrix = upsetPoints){

  stratWins <- rep(0, 14)
  stratPlace <- rep(0, 14)
  stratShow <- rep(0, 14)
  stratMatrix <- matrix(cbind(myPointsVector[resultIndex], upsetPointsMatrix[resultIndex,]), nrow = maxIter)

  rankMatrix <- apply(stratMatrix, 2, rankVinM_Q, pointsMtrx = totalPointsMatrix)
  stratWins <- colSums(rankMatrix[, 1:14] == 1)
  stratPlace <- colSums(rankMatrix[, 1:14] == 2)
  stratShow <- colSums(rankMatrix[, 1:14] == 3)

  resultsMatrix <<- as.matrix(cbind(stratWins, stratPlace, stratShow), nrow = 6, ncol = 3) * 17.0 / maxIter
  winnings <<- round(as.data.frame(t((resultsMatrix %*% payouts))), 1)
  inTheMoney <<- round(rowSums(resultsMatrix %*% (1*(payouts > 0))), 2)

  colnames(winnings) <<- c("WTP", "Fav", "Fav-1", "Fav-2", "Fav-3", "Fav-4",
                           "Fav-5", "Fav-6", "Fav-7", "Fav-8", "Fav-9",
                           "Fav-10", "Fav-11", "Fav-12")
  rownames(resultsMatrix) <<- colnames(winnings)
  #print(resultsMatrix)
  #   print(rbind(round(winnings, 2), round(apply(resultsMatrix, 1, sum), 1)))
  #   cat(paste0("maxIterations = ", maxIter))

}

