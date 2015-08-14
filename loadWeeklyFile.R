###
rm(list = ls())
load("fansimsSkeleton.RData")
source("data_to_load.R") #getwd()

processFile("2014week15.csv")
genMtx() #strategies
simParams()
littleSim()

# calcWinners(250)
# rankMatrix <- littleSim(numFans = 250, totalPointsMatrix = totalPointsIter,
#                       upsetPointsMatrix = upsetPoints)
save.image("useWeeklyFile.RData")
