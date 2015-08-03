# rm(list = ls())
# load("fansimsSkeleton.RData")
# source("data_to_load.R") #getwd()
# 
# processFile("2014week16.csv")
# 
# genMtx()
# simParams()
# save.image("useWeeklyFile.RData")

rm(list = ls())
library(shiny)
load("useWeeklyFile.RData")
gameRanks <- games:1

# Define a server for the Shiny app
shinyServer(function(input, output) {
    
  reactive(simulatePool(numFans = input$players, payouts = c(input$first, input$second, input$third)))
  
  # Filter data based on selections
  output$table <- renderTable({
    
    gameRanks
#     data <- if (input$select == "Top Payouts"){
#       cbind(gameRanks, strategies[, topWin])
#     } else if (input$select == "Top Winners"){
#       cbind(gameRanks, strategies[, topMoney])
#     } else if (input$select == "Favorites"){
#       cbind(gameRanks, favorites)
#     } else {
#       gameRanks
#     }
#     data
  }, drop = FALSE)
  
})
