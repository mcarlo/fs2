# Script for simulating fan picks each week
# p players, g games
# n sims, g games
# 

library("rbenchmark")

setwd("D:/WTP")
weekFile <- read.csv("2014week16.csv", stringsAsFactors = F)
weekFile <- weekFile[order(-weekFile$Confidence),]
winProb <- weekFile$WinProbability

games <- length(winProb)
premium <- 16 - games

simFanPicks <- function(maxIter, group = "Red", prem = FALSE, players = 82){
  if (group == "Ted"){
    payouts <- c(100, 75, 50)
  } else if (group == "Jack"){
    payouts <- c(220, 125, 55)
  } else {
    payouts <- c(100, 0, 0)
  }


  fanProb <- matrix(rep(weekFile$FanProb, players), nrow = games, ncol = players)
  favConf <- matrix(rep(weekFile$FavConf, players), nrow = games, ncol = players)
  dogConf <- matrix(rep(weekFile$DogConf, players), nrow = games, ncol = players)
  simRaw <- matrix(rep(0,players*games), nrow = games, ncol = players)

  premiumPts <- 0 + prem * premium
  stratWins <- rep(0, 11)
  stratPlace <- stratWins
  stratShow <- stratWins
  set.seed(as.numeric(Sys.time()))
  

  oneupsetmatrix <- matrix(rep(1, games**2), nrow = games, ncol = games)
  diag(oneupsetmatrix) <- rep(0, games)
  
  oneupsetConfidence <- oneupsetmatrix
  for (j in 1:games){
    for (i in 1:games){
      if (i < j) {
        oneupsetConfidence[i, j] <- games + premiumPts - i      
      } else {
        oneupsetConfidence[i, j] <- games + premiumPts - i + 1
      }
    }
  }
  diag(oneupsetConfidence) <- rep(games + premiumPts, games)
  

  for (i in 1:maxIter){
    simPlayers <- matrix(runif(games*players), nrow = games, ncol = players)
    
    simPicks <- matrix((simPlayers < fanProb)*1, nrow = games, ncol = players)
    simRand <- matrix(runif(games*players), nrow = games, ncol = players)
    simRanks <- matrix(rep(0, games*players), nrow = games, ncol = players)
    
    simFavs <- simRaw; simDogs <- simRaw; simPrior <- simRaw;
    
    simFavs <- matrix(qbinom(simRand, 16, favConf/16, lower.tail = T), nrow = games, ncol = players) + (runif(players * games) - .5)
    simDogs <- matrix(qbinom(simRand, 16, dogConf/16, lower.tail = T), nrow = games, ncol = players) + (runif(players * games) - .5)
    simPrior <- matrix(qbinom(simRand, 16, 0.5, lower.tail = T), nrow = games, ncol = players) + (runif(players * games) - .5)
    
    simRaw <- (simPrior + simFavs *simPicks + simDogs *(1 - simPicks))/2
    
    simRanks <- apply(simRaw, 2, rank) + premiumPts
    
    simOutcomes <- 1*(runif(games) <= winProb); # simOutcomes[5] <- 0
    myRanks <- rank(winProb)+premiumPts
    
    myCorrect <- simOutcomes * myRanks
    myPoints <- sum(myCorrect)
    simCorrect <- simOutcomes * simPicks + (1 - simOutcomes) * (1 - simPicks)
    totalPoints <- apply(simCorrect * simRanks, 2, sum)
    WTP <- 1*(myPoints >= max(totalPoints))
    
    upsetPoints <- apply(((simOutcomes * oneupsetmatrix) + (simOutcomes - 1) * (oneupsetmatrix - 1)) * oneupsetConfidence, 2, sum)
    
    WTP <- 1*(myPoints >= max(totalPoints))
    opp1Win <- 1*(upsetPoints[1] > max(totalPoints))
    opp2Win <- 1*(upsetPoints[2] > max(totalPoints))
    opp3Win <- 1*(upsetPoints[3] > max(totalPoints))
    opp4Win <- 1*(upsetPoints[4] > max(totalPoints))
    opp5Win <- 1*(upsetPoints[5] > max(totalPoints))

    opp6Win <- 1*(upsetPoints[6] > max(totalPoints))
    opp7Win <- 1*(upsetPoints[7] > max(totalPoints))
    opp8Win <- 1*(upsetPoints[8] > max(totalPoints))
    opp9Win <- 1*(upsetPoints[9] > max(totalPoints))
    opp10Win <- 1*(upsetPoints[10] > max(totalPoints))
    
    WTPplace <- 1*(sum(myPoints < totalPoints) == 1)
    opp1Place <- 1*(sum(upsetPoints[1] < totalPoints) == 1)
    opp2Place <- 1*(sum(upsetPoints[2] < totalPoints) == 1)
    opp3Place <- 1*(sum(upsetPoints[3] < totalPoints) == 1)
    opp4Place <- 1*(sum(upsetPoints[4] < totalPoints) == 1)
    opp5Place <- 1*(sum(upsetPoints[5] < totalPoints) == 1)
    
    opp6Place <- 1*(sum(upsetPoints[6] < totalPoints) == 1)
    opp7Place <- 1*(sum(upsetPoints[7] < totalPoints) == 1)
    opp8Place <- 1*(sum(upsetPoints[8] < totalPoints) == 1)
    opp9Place <- 1*(sum(upsetPoints[9] < totalPoints) == 1)
    opp10Place <- 1*(sum(upsetPoints[10] < totalPoints) == 1)
    
    WTPShow <- 1*(sum(myPoints < totalPoints) == 2)
    opp1Show <- 1*(sum(upsetPoints[1] < totalPoints) == 2)
    opp2Show <- 1*(sum(upsetPoints[2] < totalPoints) == 2)
    opp3Show <- 1*(sum(upsetPoints[3] < totalPoints) == 2)
    opp4Show <- 1*(sum(upsetPoints[4] < totalPoints) == 2)
    opp5Show <- 1*(sum(upsetPoints[5] < totalPoints) == 2)
    
    opp6Show <- 1*(sum(upsetPoints[6] < totalPoints) == 2)
    opp7Show <- 1*(sum(upsetPoints[7] < totalPoints) == 2)
    opp8Show <- 1*(sum(upsetPoints[8] < totalPoints) == 2)
    opp9Show <- 1*(sum(upsetPoints[9] < totalPoints) == 2)
    opp10Show <- 1*(sum(upsetPoints[10] < totalPoints) == 2)


    temp <- c(WTP, opp1Win, opp2Win, opp3Win, opp4Win, opp5Win, opp6Win, opp7Win, opp8Win, opp9Win, opp10Win)
    stratWins <- stratWins + temp

    tempPlace <- c(WTPplace, opp1Place, opp2Place, opp3Place, opp4Place,
                   opp5Place, opp6Place, opp7Place, opp8Place, opp9Place,
                   opp10Place)
    stratPlace <- stratPlace + tempPlace
    
    tempShow <- c(WTPShow, opp1Show, opp2Show, opp3Show, opp4Show, opp5Show,
                  opp6Show, opp7Show, opp8Show, opp9Show, opp10Show)
    stratShow <- stratShow + tempShow
    
    temp <- rep(0, 11)
    tempPlace <- temp
    tempShow <- temp
    
  }
  

  resultsMatrix <- as.matrix(cbind(stratWins, stratPlace, stratShow), nrow = 6, ncol = 3)
  winnings <- as.data.frame(t((resultsMatrix %*% payouts))/(1.0 * maxIter))
  colnames(winnings) <- c("WTP", "Fav", "Fav-1", "Fav-2", "Fav-3", "Fav-4",
                          "Fav-5", "Fav-6", "Fav-7", "Fav-8", "Fav-9")
  rownames(resultsMatrix) <- colnames(winnings)
  print(resultsMatrix)
  print(round(winnings, 2))
  if (group == "Red"){
    print(resultsMatrix[,1])
  } else {
    print(apply(resultsMatrix, 1, sum))
  }
}

system.time(simFanPicks(300))
system.time(simFanPicks(300, group = "Ted", players = 97))
simFanPicks(1000, group = "Jack", players = 162, prem = TRUE)
weekFile
