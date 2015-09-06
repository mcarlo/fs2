###
rm(list = ls())
load("fansimsSkeleton.RData")
load("altStuff.RData")
source("data_to_load.R") #getwd()

processFile("D:/WTP/WEEK01_2015.csv") #"2014week15.csv")
genMtx() #strategies
simParams()
littleSim()

popConfList <- function(size){list(size, calcWinners(size))}
results05 <- popConfList(5)

fanSizes <- seq(5, 100, by = 5)

resultsLists <- rep(results05, 20)

confTactics <- function(){
  for(i in 2:20)  {
    
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
