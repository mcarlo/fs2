# Create placeholder objects to allocate memory

rm(list= ls())
playerCols = 500
weekseed = 1
resultIndex <- sample(1:2000, 2000, replace = TRUE)

games = 13

# simulate placeholder
simRaw13 <- matrix(rep(0,playerCols*games), nrow = games, ncol = playerCols)


upsetDiagMatrix13 <- matrix(rep(0, games * games), nrow = games,
                          ncol = games)
diag(upsetDiagMatrix13) <- games

upsetMatrix13 <- matrix(rep((games:1), games, times = games),
                      nrow = games, ncol = games)

diag(upsetMatrix13) <- 0

for (j in 2:games){
  upsetMatrix13[1:(j-1), j] <- (games - 1):
    (games - j + 1)
}

simplayerCols13 <- matrix(runif(games*playerCols), nrow = games, ncol = playerCols)
#simPicks13 <- simplayerCols * 0

simRand13 <- matrix(runif(games*playerCols), nrow = games, ncol = playerCols)
simOutcomes2_13 <- matrix(runif(games * 2000), ncol = 2000)

simPicks13 <- matrix((simplayerCols13 < .5)*1, nrow = games, ncol = playerCols)
simFavs13 <- matrix(qbinom(simRand13[1:games,], games, (.5)/games, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
simDogs13 <- matrix(qbinom(simRand13[1:games,], games, (.5)/games, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
simPrior13 <- matrix(qbinom(simRand13[1:games,], games, 0.5, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
simRaw13 <- (simPrior13 + simFavs13 *simPicks13 + simDogs13 *(1 - simPicks13))/2

fanIndex13 <- sample(1:playerCols, playerCols, replace = T)

games = 14

# simulate placeholder
simRaw14 <- matrix(rep(0,playerCols*games), nrow = games, ncol = playerCols)


upsetDiagMatrix14 <- matrix(rep(0, games * games), nrow = games,
                             ncol = games)
diag(upsetDiagMatrix14) <- games

upsetMatrix14 <- matrix(rep((games:1), games, times = games),
                         nrow = games, ncol = games)

diag(upsetMatrix14) <- 0

for (j in 2:games){
  upsetMatrix14[1:(j-1), j] <- (games - 1):
    (games - j + 1)
}

simplayerCols14 <- matrix(runif(games*playerCols), nrow = games, ncol = playerCols)
#simPicks14 <- simplayerCols * 0

simRand14 <- matrix(runif(games*playerCols), nrow = games, ncol = playerCols)
simOutcomes2_14 <- matrix(runif(games * 2000), ncol = 2000)

simPicks14 <- matrix((simplayerCols14 < .5)*1, nrow = games, ncol = playerCols)
simFavs14 <- matrix(qbinom(simRand14[1:games,], games, (.5)/games, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
simDogs14 <- matrix(qbinom(simRand14[1:games,], games, (.5)/games, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
simPrior14 <- matrix(qbinom(simRand14[1:games,], games, 0.5, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
simRaw14 <- (simPrior14 + simFavs14 *simPicks14 + simDogs14 *(1 - simPicks14))/2

fanIndex14 <- sample(1:playerCols, playerCols, replace = T)


games = 15

# simulate placeholder
simRaw15 <- matrix(rep(0,playerCols*games), nrow = games, ncol = playerCols)


upsetDiagMatrix15 <- matrix(rep(0, games * games), nrow = games,
                             ncol = games)
diag(upsetDiagMatrix15) <- games

upsetMatrix15 <- matrix(rep((games:1), games, times = games),
                         nrow = games, ncol = games)

diag(upsetMatrix15) <- 0

for (j in 2:games){
  upsetMatrix15[1:(j-1), j] <- (games - 1):
    (games - j + 1)
}

simplayerCols15 <- matrix(runif(games*playerCols), nrow = games, ncol = playerCols)
#simPicks15 <- simplayerCols * 0

simRand15 <- matrix(runif(games*playerCols), nrow = games, ncol = playerCols)
simOutcomes2_15 <- matrix(runif(games * 2000), ncol = 2000)

simPicks15 <- matrix((simplayerCols15 < .5)*1, nrow = games, ncol = playerCols)
simFavs15 <- matrix(qbinom(simRand15[1:games,], games, (.5)/games, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
simDogs15 <- matrix(qbinom(simRand15[1:games,], games, (.5)/games, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
simPrior15 <- matrix(qbinom(simRand15[1:games,], games, 0.5, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
simRaw15 <- (simPrior15 + simFavs15 *simPicks15 + simDogs15 *(1 - simPicks15))/2

fanIndex15 <- sample(1:playerCols, playerCols, replace = T)


games = 16

# simulate placeholder
simRaw16 <- matrix(rep(0,playerCols*games), nrow = games, ncol = playerCols)


upsetDiagMatrix16 <- matrix(rep(0, games * games), nrow = games,
                             ncol = games)
diag(upsetDiagMatrix16) <- games

upsetMatrix16 <- matrix(rep((games:1), games, times = games),
                         nrow = games, ncol = games)

diag(upsetMatrix16) <- 0

for (j in 2:games){
  upsetMatrix16[1:(j-1), j] <- (games - 1):
    (games - j + 1)
}

simplayerCols16 <- matrix(runif(games*playerCols), nrow = games, ncol = playerCols)
#simPicks16 <- simplayerCols * 0

simRand16 <- matrix(runif(games*playerCols), nrow = games, ncol = playerCols)
simOutcomes2_16 <- matrix(runif(games * 2000), ncol = 2000)

simPicks16 <- matrix((simplayerCols16 < .5)*1, nrow = games, ncol = playerCols)
simFavs16 <- matrix(qbinom(simRand16[1:games,], games, (.5)/games, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
simDogs16 <- matrix(qbinom(simRand16[1:games,], games, (.5)/games, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
simPrior16 <- matrix(qbinom(simRand16[1:games,], games, 0.5, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
simRaw16 <- (simPrior16 + simFavs16 *simPicks16 + simDogs16 *(1 - simPicks16))/2

fanIndex16 <- sample(1:playerCols, playerCols, replace = T)

save.image("fansimsSkeleton.RData")
