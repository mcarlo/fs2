###
rm(list = ls())
load("fansimsSkeleton.RData")
source("data_to_load.R") #getwd()

processFile("2014week17.csv")

genMtx()
simParams()
simulatePool(numFans = 250, payouts = c(225, 125, 50))
strategies[, topWin]
strategies[, topMoney]
favorites

