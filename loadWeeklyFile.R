###
rm(list = ls())
load("fansimsSkeleton.RData")
load("altStuff.RData")
source("data_to_load.R") #getwd()

processFile("~/WEEK01_2015.csv")  #("D:/WTP/WEEK01_2015.csv") #"2014week15.csv")
genMtx() #strategies
simParams()
littleSim()

popConfList <- function(size){list(size, calcWinners(size))}
results05 <- popConfList(5)



resultsLists <- rep(results05, 20)

confTactics <- function(startList, maxSize = 100){
  # maxSize must be divisible by 5
  # startList <- results05
  # maxSize <- 100

  fanSizes <- seq(5, maxSize, by = 5)
  maxIter <- maxSize/5
  outList <- rep(startList, maxIter)

  for(i in 1:maxIter)  { #i=1

    size <- fanSizes[i]
    genList <- popConfList(size)
    outList[[2*(i - 1) + 1]] <- genList[[1]]
    outList[[2*i]] <- genList[[2]]

  }
  outList
}
system.time(resultsLists <- confTactics(results05, 250))

save(resultsLists, gameRanks, weekFileConf, file = "useWeeklyFile.RData")
