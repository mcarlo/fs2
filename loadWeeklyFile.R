processFile <- function(weekFilename){
  #rm(list = ls())
  load("fansimsSkeleton.RData")
  #weekFilename = "2014week11.csv"
  # these objects will vary by week
  # 
  
  setwd("D:/Documents/GitHub/fs2")

  weekFile <- read.csv(weekFilename, stringsAsFactors = F)
  weekFile <<- weekFile[order(-weekFile$Confidence),]
  winProb <<- weekFile[, 2]
  if (max(winProb) > 1) {winProb <<- winProb/100.0}
  
  favorites <<- weekFile$Victor
  strategies <<- matrix(rep(favorites, 14), ncol = 14)
  
  # simulate whether fans pick the favorite
  fanProb <- weekFile$FanProb
  
  # simulate favorite confidence and underdog confidence
  favConf <- weekFile$FavConf
  dogConf <- weekFile$DogConf
  
  games <<- length(winProb)
  premium <- 16 - games
  prem <- FALSE
  premiumPts <- 0 + prem * premium
  
  oppLabel <- function(c){paste0(c, "'s opponent")}
  dogs <<- sapply(favorites, oppLabel)
  if(dim(weekFile)[2] == 8) {dogs <<- weekFile$Underdog}
  
  selectRows <- (1:games) 
  selectRowsPrem <- selectRows + (1 - prem) * premium
  for (j in 2:14){ # j = 2
    strategies[j - 1, j] <- dogs[j-1] #weekFile[1:3, ]; favorites[j]
    strategies[, j] <- strategies[order(-(upsetMatrix[selectRowsPrem, selectRowsPrem] + upsetDiagMatrix[selectRowsPrem, selectRowsPrem])[ , j - 1]), j]
  }
  strategies <<- strategies
  
  ### dependent matrices
  simPicks <- matrix((simplayerCols[selectRows, ]  < fanProb)*1, nrow = games, ncol = playerCols)
  
  simFavs <- matrix(qbinom(simRand[1:games,], games, (favConf - .5)/games, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
  # simFavs[1:10, 1:10]
  
  simDogs <- matrix(qbinom(simRand[1:games,], games, (dogConf - .5)/games, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
  simPrior <- matrix(qbinom(simRand[1:games,], games, 0.5, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
  #rm(simplayerCols); rm(simRand)
  
  simRaw <- (simPrior + simFavs *simPicks + simDogs *(1 - simPicks))/2
  
  simRanks <- apply(simRaw[selectRows,], 2, rank) + premiumPts # max(apply(simRanks, 2, max))
  #rm(simRaw); rm(simPrior); rm(simFavs); rm(simDogs)
  
  simOutcomes2 <<- (simOutcomes2[selectRows,] <= winProb) * 1
  
  myRanks <<- rank(winProb, ties.method = "random")+premiumPts
  
  myPoints <<- as.vector(crossprod(myRanks, simOutcomes2)) # * myRanks
  
  
  totalPoints <<- t(crossprod((simPicks * simRanks), simOutcomes2) + 
                      crossprod((1 - simPicks) * simRanks, (1 - simOutcomes2)))
  
  upsetPoints <<- t(crossprod(upsetMatrix[selectRowsPrem,selectRowsPrem], simOutcomes2) + 
                      crossprod(upsetDiagMatrix[selectRowsPrem,selectRowsPrem], (1 - simOutcomes2)))
  
}

library(compiler)
cmpProc <- cmpfun(processFile)

system.time(processFile("2014week17.csv"))
system.time(cmpProc("2014week17.csv"))
