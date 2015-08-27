###
rm(list = ls())
load("fansimsSkeleton.RData")
load("altStuff.RData")
source("data_to_load.R") #getwd()

processFile("2015week01.csv")
genMtx() #strategies
simParams()
littleSim()

popConfList <- function(size){list(size, calcWinners(size))}
results05 <- popConfList(5)

fanSizes <- seq(5, 250, by = 5)

resultsLists <- rep(results05, 50)

confTactics <- function(){
  for(i in 2:50)  {
    
    size <- fanSizes[i]
    genList <- popConfList(size)
    resultsLists[[2*(i - 1) + 1]] <<- genList[[1]]
    resultsLists[[2*i]] <<- genList[[2]]
    
  }
}
confTactics()
resultsLists

# calcWinners(250)
# rankMatrix <- littleSim(numFans = 250, totalPointsMatrix = totalPointsIter,
#                       upsetPointsMatrix = upsetPoints)
save.image("useWeeklyFile.RData")
