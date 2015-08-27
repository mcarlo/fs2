rm(list = ls())
genGameRanks <- function(nGames = 16) {nGames:1}

# generate square matrix with each column == gameRanks

genSq <- function(nGames = 16){
  matrix(rep(genGameRanks(nGames), times = nGames), ncol = nGames)
}

#genSq(14)

genZeroSq <- function(nGames = 16){
  matrix(rep(0, nGames^2), ncol = nGames)
}

nGames = 16
altFavs <- genZeroSq(nGames)
altUpsets <- altFavs
altUpsetsRow <- altUpsets
startRanks <- genSq(nGames)

topCol = 5

altUpsets <- startRanks
for (j in 1:nGames){ #j = 1
  threshold <- (nGames + 1 - j)
  if (j < topCol){
    altUpsets[j:(topCol - 1) ,j] <- startRanks[(j + 1):topCol ,j] 
  }
  if (j > topCol){
    altUpsets[(topCol + 1):j ,j] <- startRanks[topCol:(j-1) ,j] 
  }
}
altUpsets[topCol, ] <- genGameRanks(16)

altUpsets2 <- altUpsets
altUpsets2[topCol, ] <- 0
altUpsetsRow[topCol, ] <- genGameRanks()

ls()
save.image("altStuff.RData")
