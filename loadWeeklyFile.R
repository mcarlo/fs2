###
rm(list = ls())
load("fansimsSkeleton.RData")
source("data_to_load.R") #getwd()

processFile("2014week14.csv")
games
genMtx()
simParams()
save.image("useWeeklyFile.RData")

simulatePool(numFans = 2, payouts = c(225, 125, 50))
topWin
topMoney
strategies[, topWin]
strategies[, topMoney]
favorites

