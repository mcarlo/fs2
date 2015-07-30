###
rm(list = ls())
load("fansimsSkeleton.RData")
source("data_to_load.R") #getwd()

processFile("2014week16.csv")

genMtx()
mem_change(simulatePool(numFans = 25, payouts = c(225, 125, 50)))
strategies[, topWin]
strategies[, topMoney]
favorites

system.time(cmpsim(numFans = 25, payouts = c(225, 125, 50)))
strategies[, topWin]
strategies[, topMoney]
favorites
