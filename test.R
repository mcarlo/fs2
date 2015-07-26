load("fsims2.RData")

simulatePool(maxIter = 2000 ,numFans = input$players, 
             payouts = c(input$first, input$second, input$third))

topWin <- order(-winnings[1,])[1:3]
topMoney <- order(-inTheMoney)[1:3]
data <- cbind(myRanks[order(-myRanks)] , favorites)