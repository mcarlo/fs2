# fansims2_prototype

# Set directory and read in files
#
setup <- function(weekFilename){
#setupComplete = FALSE

  setwd("C:/Users/Anichini/Documents")
  #setwd("D:/WTP")

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

  myRanks <- rank(winProb)+premiumPts

  myPoints <<- as.vector(t(myRanks) %*% simOutcomes2) # * myRanks


  totalPoints <<- t(t(simPicks * simRanks) %*% simOutcomes2 + t((1 - simPicks) * simRanks) %*% (1 - simOutcomes2))

  upsetPoints <<- t(t(upsetMatrix) %*% simOutcomes2 + t(upsetDiagMatrix) %*% (1 - simOutcomes2))

  fanIndex <- sample(1:2000, 100, replace = T)

  setupComplete = TRUE

}

rankN <- function(x, n) {x[order(x, decreasing = T)][n]}
YrankN <- function(x, y, n) {sum(x > y) == n - 1}

simParams <- function(maxiter = 2000, numFans = 90){
  suppressMessages(require(foreach))
  resultIndex <<- sample(1:2000, maxiter, replace = TRUE)
  fanIndex <<- foreach(resultIndex, .combine = rbind) %do% sample(1:2000, numFans, replace = T)
  totalPointsIter <<- matrix(foreach(i = 1:2000, .combine = rbind) %do%
    totalPoints[resultIndex[i], fanIndex[i,]], nrow = 2000, ncol = numFans)
}

simulatePool <- function(maxIter = 2000, numFans = 90,
                         payouts = c(100, 0, 0), totalPointsMatrix = totalPoints,
                         myPointsVector = myPoints, upsetPointsMatrix = upsetPoints){

  stratWins <- rep(0, 14)
  stratPlace <- rep(0, 14)
  stratShow <- rep(0, 14)

#   temp <- rep(0, 14)
#   tempPlace <- rep(0, 14)
#   tempShow <- rep(0, 14)

  suppressMessages(require(parallel)); suppressMessages(require(doParallel))

  WTP <- 1*(myPointsVector[resultIndex] > apply(totalPointsIter, 1, max))
  opp1Win <- 1*(upsetPointsMatrix[resultIndex, 1] > apply(totalPointsIter, 1, max))
  opp2Win <- 1*(upsetPointsMatrix[resultIndex, 2] > apply(totalPointsIter, 1, max))
  opp3Win <- 1*(upsetPointsMatrix[resultIndex, 3] > apply(totalPointsIter, 1, max))
  opp4Win <- 1*(upsetPointsMatrix[resultIndex, 4] > apply(totalPointsIter, 1, max))
  opp5Win <- 1*(upsetPointsMatrix[resultIndex, 5] > apply(totalPointsIter, 1, max))

  opp6Win <- 1*(upsetPointsMatrix[resultIndex, 6] > apply(totalPointsIter, 1, max))
  opp7Win <- 1*(upsetPointsMatrix[resultIndex, 7] > apply(totalPointsIter, 1, max))
  opp8Win <- 1*(upsetPointsMatrix[resultIndex, 8] > apply(totalPointsIter, 1, max))
  opp9Win <- 1*(upsetPointsMatrix[resultIndex, 9] > apply(totalPointsIter, 1, max))
  opp10Win <- 1*(upsetPointsMatrix[resultIndex, 10] > apply(totalPointsIter, 1, max))

  opp11Win <- 1*(upsetPointsMatrix[resultIndex, 11] > apply(totalPointsIter, 1, max))
  opp12Win <- 1*(upsetPointsMatrix[resultIndex, 12] > apply(totalPointsIter, 1, max))
  opp13Win <- 1*(upsetPointsMatrix[resultIndex, 13] > apply(totalPointsIter, 1, max))

  stratWins <- apply(cbind(WTP, opp1Win, opp2Win, opp3Win, opp4Win, opp5Win, opp6Win,
                 opp7Win, opp8Win, opp9Win, opp10Win, opp11Win, opp12Win, opp13Win), 2, sum)

  rankN <- function(x, n) {x[order(x, decreasing = T)][n]}
  YrankN <- function(x, y, n) {sum(x > y) == n - 1}

  cl = makePSOCKcluster(2)
  registerDoParallel(cl, cores = 3)
  tpm <- totalPointsIter
  myP <- myPoints
  WTPplace <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = myP[i], n = 2) * 1

  opp1Place <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 1], n = 2) * 1

  opp2Place <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 2], n = 2) * 1

  opp3Place <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 3], n = 2) * 1

  opp4Place <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 4], n = 2) * 1

  opp5Place <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 5], n = 2) * 1

  opp6Place <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 6], n = 2) * 1

  opp7Place <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 7], n = 2) * 1

  opp8Place <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 8], n = 2) * 1

  opp9Place <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 9], n = 2) * 1

  opp10Place <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 10], n = 2) * 1

  opp11Place <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 11], n = 2) * 1

  opp12Place <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 12], n = 2) * 1

  opp13Place <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 13], n = 2) * 1

  stratPlace <- apply(cbind(WTPplace, opp1Place, opp2Place, opp3Place, opp4Place, opp5Place, opp6Place, opp7Place, opp8Place, opp9Place, opp10Place, opp11Place, opp12Place, opp13Place), 2, sum)


  WTPShow <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = myP[i], n = 3) * 1

  opp1Show <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 1], n = 3) * 1

  opp2Show <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 2], n = 3) * 1

  opp3Show <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 3], n = 3) * 1

  opp4Show <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 4], n = 3) * 1

  opp5Show <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 5], n = 3) * 1

  opp6Show <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 6], n = 3) * 1

  opp7Show <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 7], n = 3) * 1

  opp8Show <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 8], n = 3) * 1

  opp9Show <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 9], n = 3) * 1

  opp10Show <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 10], n = 3) * 1

  opp11Show <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 11], n = 3) * 1

  opp12Show <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 12], n = 3) * 1

  opp13Show <- foreach(i = 1:2000, .combine = c) %dopar% YrankN(tpm[i,], y = upsetPointsMatrix[i, 13], n = 3) * 1

  stratShow <- apply(cbind(WTPShow, opp1Show, opp2Show, opp3Show, opp4Show, opp5Show, opp6Show, opp7Show, opp8Show, opp9Show, opp10Show, opp11Show, opp12Show, opp13Show), 2, sum)

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
  inTheMoney[which(rank(inTheMoney) == 14) ]
  inTheMoney[which(rank(inTheMoney) == 13) ]
  inTheMoney[which(rank(inTheMoney) == 12) ]
}

top3Dollars <- function(){
  winnings[which(rank(winnings) == 14)]
  winnings[which(rank(winnings) == 13)]
  winnings[which(rank(winnings) == 12)]
}

userPicks <- function(picksVector){
  #picksVector <- jokerPicks
  userFavs <- (picksVector %in% favorites) * 1
  userDogs <- (picksVector %in% dogs) * 1
  userPoints <<- t(userFavs * (games + premiumPts):(premiumPts+1)) %*% simOutcomes2 + t(userDogs * (games + premiumPts):(premiumPts+1)) %*% (simOutcomes2  - 1)

}

save.image("fsims2.RData")

###
# rm(list = ls())
load("fsims2.RData")

# setup("2014week11.csv") # 2000 sufficient
# setup("2014week12.csv") # 2000 sufficient
# setup("2014week13.csv") # 2000 sufficient
# setup("2014week14.csv") # 2000 sufficient
# setup("2014week15.csv") # 2000 sufficient
# setup("2014week16.csv") # 20000 sufficient
setup("2014week17.csv") # 20000 sufficient

simParams(numFans = 190)
system.time(simulatePool(maxIter = 2000, numFans = 190, payouts = c(220, 100, 50)))

topWin <- which(winnings == max(winnings))
topMoney <- which(inTheMoney == max(inTheMoney))
strategies[, topWin]
strategies[, topMoney]
favorites
