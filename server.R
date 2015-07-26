library(shiny)

load("fsims2.RData")

#input <- data.frame(players = 190, first = 220, second = 125, third = 50 )

simulatePool(maxIter = 2000 ,numFans = input$players, 
             payouts = c(input$first, input$second, input$third))

topWin <- order(-winnings[1,])[1:3]
topMoney <- order(-inTheMoney)[1:3]
# data <- cbind(myRanks[order(-myRanks)] , strategies[,topWin])
# data
# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  
  # Filter data based on selections
  output$table <- renderDataTable({
    
#     if (input$select == "Top Payouts"){
#       data <- strategies[, topWin]
#     } else if (input$select == "Top Winners"){
#       data <- strategies[, topMoney]
#     } else {
#       data <- favorites
#     }
    data <- data.frame(cbind(favorites, strategies[, topWin], strategiest[, topMoney]))
    data
  })
  
})
