# Create placeholder objects to allocate memory

# rm(list= ls())
library(pryr)
mem_used()

mem_change(playerCols <- 2000)
mem_change(games <- 16)

# simulate placeholder
mem_change(simRaw <- matrix(rep(0,playerCols*games), nrow = games, ncol = playerCols))


mem_change(upsetDiagMatrix <<- matrix(rep(0, games * games), nrow = games,
                          ncol = games))
mem_change(diag(upsetDiagMatrix) <- games)

mem_change(upsetMatrix <<- matrix(rep((games:1), games, times = games),
                      nrow = games, ncol = games))

mem_change(diag(upsetMatrix) <- 0)

mem_change(for (j in 2:games){
  upsetMatrix[1:(j-1), j] <- (games - 1):
    (games - j + 1)
})

mem_change(set.seed(123)) #as.numeric(Sys.time()))

mem_change(simplayerCols <- matrix(runif(games*playerCols), nrow = games, ncol = playerCols))
mem_change(simPicks <- simplayerCols * 0)

simRand <- matrix(runif(games*playerCols), nrow = games, ncol = playerCols)

simOutcomes2 <- matrix(runif(32000), ncol = 2000)

simPicks <- matrix((simplayerCols < .5)*1, nrow = games, ncol = playerCols)
simFavs <- matrix(qbinom(simRand[1:games,], games, (.5)/games, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
simDogs <- matrix(qbinom(simRand[1:games,], games, (.5)/games, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
simPrior <- matrix(qbinom(simRand[1:games,], games, 0.5, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
simRaw <- (simPrior + simFavs *simPicks + simDogs *(1 - simPicks))/2

fanIndex <- sample(1:2000, 2000, replace = T)
resultIndex <<- sample(1:2000, 2000, replace = TRUE)
save.image("fansimsSkeleton.RData")
