library(shiny)

load("fsims2.RData")

# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  # Filter data based on selections
  output$table <- renderDataTable({
    load("fsims2.RData")
    
    simulatePool(maxIter = 2000 ,numFans = input$players, payouts = c(input$first, input$second, input$third))
    
    topWin <- order(-winnings[1,])[1:3]
    topMoney <- order(-inTheMoney)[1:3]
    data <- cbind(myRanks[order(-myRanks)] , favorites)
    
#     if (input$select == "Top Payouts"){
#       data <- strategies[, topWin]
#     } else if (input$select == "Top Winners"){
#       data <- strategies[, topMoney]
#     } else {
#       data <- favorites
#     }
    data
  })
  
})
