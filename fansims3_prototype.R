# fansims2_prototype

# Set directory and read in files
#
setup <- function(weekFilename){
#setupComplete = FALSE

  #setwd("C:/Users/Anichini/Documents")
  setwd("D:/WTP")

  ### set up weekFilename = "2014week17.csv"
  weekFile <- read.csv(weekFilename, stringsAsFactors = F)
  weekFile <<- weekFile[order(-weekFile$Confidence),]
  winProb <<- weekFile[, 2]
  if (max(winProb) > 1) {winProb <<- winProb/100.0}

  favorites <<- weekFile$Victor

  # simulate whether fans pick the favorite
  fanProb <- weekFile$FanProb

  # simulate favorite confidence and underdog confidence
  favConf <- weekFile$FavConf
  dogConf <- weekFile$DogConf

  games <<- length(winProb)
  premium <- 16 - games
  prem <- FALSE

  oppLabel <- function(c){paste0(c, "'s opponent")}
  dogs <<- sapply(favorites, oppLabel)
  if(dim(weekFile)[2] == 8) {dogs <<- weekFile$Underdog}

  playerCols = 2000

  # simulate placeholder
  simRaw <- matrix(rep(0,playerCols*games), nrow = games, ncol = playerCols)

  premiumPts <- 0 + prem * premium

  upsetDiagMatrix <- matrix(rep(0, games * games), nrow = games,
                            ncol = games)
  diag(upsetDiagMatrix) <- rep(games + premiumPts, games)

  upsetMatrix <- matrix(rep((games:1) + premiumPts, games, times = games),
                        nrow = games, ncol = games)

  diag(upsetMatrix) <- rep(0, games)

  for (j in 2:games){
    upsetMatrix[1:(j-1), j] <- (games + premiumPts - 1):
      (games + premiumPts - j + 1)
  }

  strategies <<- matrix(rep(favorites, 14), ncol = 14)
  for (j in 2:14){ # j = 2
    strategies[j - 1, j] <- dogs[j-1] #weekFile[1:3, ]; favorites[j]
    strategies[, j] <<- strategies[order(-(upsetMatrix + upsetDiagMatrix)[, j - 1]), j]
  }

  set.seed(123) #as.numeric(Sys.time()))

  simplayerCols <- matrix(runif(games*playerCols), nrow = games, ncol = playerCols)

  simPicks <- matrix((simplayerCols < fanProb)*1, nrow = games, ncol = playerCols)
  # simPicks[1:10, 1:10]

  simRand <- matrix(runif(games*playerCols), nrow = games, ncol = playerCols)
  # simRanks <- matrix(rep(0, games*playerCols), nrow = games, ncol = playerCols)
  # simRanks[1:10, 1:10]

  simFavs <- matrix(qbinom(simRand, games, (favConf - .5)/games, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
  # simFavs[1:10, 1:10]

  simDogs <- matrix(qbinom(simRand, games, (dogConf - .5)/games, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
  simPrior <- matrix(qbinom(simRand, games, 0.5, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
  rm(simplayerCols); rm(simRand)

  simRaw <- (simPrior + simFavs *simPicks + simDogs *(1 - simPicks))/2

  simRanks <- apply(simRaw, 2, rank) + premiumPts # max(apply(simRanks, 2, max))
  rm(simRaw); rm(simPrior); rm(simFavs); rm(simDogs)

  simOutcomes2 <<- matrix(1*(runif(games * 2000) <= winProb), nrow = games, ncol = 2000)

  myRanks <- rank(winProb, ties.method = "random")+premiumPts

  myPoints <<- as.vector(t(myRanks) %*% simOutcomes2) # * myRanks


  totalPoints <<- t(t(simPicks * simRanks) %*% simOutcomes2 + t((1 - simPicks) * simRanks) %*% (1 - simOutcomes2))

  upsetPoints <<- t(t(upsetMatrix) %*% simOutcomes2 + t(upsetDiagMatrix) %*% (1 - simOutcomes2))

  fanIndex <- sample(1:2000, 100, replace = T)

  setupComplete = TRUE

}

simParams <- function(maxiter = 2000, numFans = 90){
  suppressMessages(require(foreach))
  resultIndex <<- sample(1:2000, maxiter, replace = TRUE)
  fanIndex <<- foreach(resultIndex, .combine = rbind) %do% sample(1:2000, numFans, replace = T)
  totalPointsIter <<- matrix(foreach(i = 1:2000, .combine = rbind) %do%
                               totalPoints[resultIndex[i], fanIndex[i,]], nrow = 2000, ncol = numFans)
}

rankVinM_Q <- function(vec = myPointsVector[resultIndex], pointsMtrx = totalPointsIter){
  temp <- -matrix(cbind(vec, pointsMtrx), ncol = dim(pointsMtrx)[2] + 1)
  rankM <- t(apply(temp, 1, rank, ties.method = "min"))[, 1]
}

simulatePool <- function(maxIter = 2000, numFans = 90,
                         payouts = c(100, 0, 0), totalPointsMatrix = totalPointsIter,
                         myPointsVector = myPoints, upsetPointsMatrix = upsetPoints){

  stratWins <- rep(0, 14)
  stratPlace <- rep(0, 14)
  stratShow <- rep(0, 14)
  stratMatrix <- matrix(cbind(myPointsVector[resultIndex], upsetPointsMatrix[resultIndex,]), nrow = maxIter)
  
  rankMatrix <- apply(stratMatrix, 2, rankVinM_Q, pointsMtrx = totalPointsMatrix)
  stratWins <- apply(rankMatrix[, 1:14] == 1, 2, sum)
  stratPlace <- apply(rankMatrix[, 1:14] == 2, 2, sum)
  stratShow <- apply(rankMatrix[, 1:14] == 3, 2, sum)

  resultsMatrix <<- as.matrix(cbind(stratWins, stratPlace, stratShow), nrow = 6, ncol = 3) * 17.0 / maxIter
  winnings <<- round(as.data.frame(t((resultsMatrix %*% payouts))), 1)
  inTheMoney <<- round(apply(resultsMatrix %*% (1*(payouts > 0)), 1, sum), 2)

  colnames(winnings) <<- c("WTP", "Fav", "Fav-1", "Fav-2", "Fav-3", "Fav-4",
                          "Fav-5", "Fav-6", "Fav-7", "Fav-8", "Fav-9",
                          "Fav-10", "Fav-11", "Fav-12")
  rownames(resultsMatrix) <<- colnames(winnings)
  #print(resultsMatrix)
  print(rbind(round(winnings, 2), round(apply(resultsMatrix, 1, sum), 1)))
  cat(paste0("maxIterations = ", maxIter))

}

top3Money <- function(){
  inTheMoney[which(rank(-inTheMoney) == 1) ]
  inTheMoney[which(rank(-inTheMoney) == 2) ]
  inTheMoney[which(rank(-inTheMoney) == 3) ]
}

top3Dollars <- function(){
  winnings[which(rank(-winnings) == 1)]
  winnings[which(rank(-winnings) == 2)]
  winnings[which(rank(-winnings) == 3)]
}

userPicks <- function(picksVector){
  #picksVector <- jokerPicks
  userFavs <- (picksVector %in% favorites) * 1
  userDogs <- (picksVector %in% dogs) * 1
  userPoints <<- t(userFavs * (games + premiumPts):(premiumPts+1)) %*% simOutcomes2 + t(userDogs * (games + premiumPts):(premiumPts+1)) %*% (simOutcomes2  - 1)

}
setup("2014week11.csv") # 20000 sufficient

simParams(numFans = 190)
simulatePool(maxIter = 2000, numFans = 190, payouts = c(220, 100, 50))


topWin <- order(-winnings[1,])[1:3]
topMoney <- order(-inTheMoney)[1:3]

save.image("fsims2.RData")

###
# rm(list = ls())
load("fsims2.RData")

simulatePool(maxIter = 2000, numFans = 190, payouts = c(220, 100, 50))


topWin <- order(-winnings[1,])[1:3]
topMoney <- order(-inTheMoney)[1:3]
strategies[, topWin]
strategies[, topMoney]
favorites
