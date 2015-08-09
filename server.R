rm(list = ls())
library(shiny)
load("useWeeklyFile.RData")
gameRanks <- games:1

# Define a server for the Shiny app
shinyServer(function(input, output) { #input <- data.frame(players = 250, first = 225, second = 125, third = 50)

  reactive({simulatePool(numFans = input$players, payouts = c(input$first, input$second, input$third))})

  # Filter data based on selections
  output$resultsTable <- renderTable({

    data <- if (input$display == "Favorites"){
      as.data.frame(cbind(gameRanks, favorites))
    } else if (input$display == "Top Payouts"){
      as.data.frame(cbind(gameRanks, strategies[, topWin]))
    } else {
      as.data.frame(cbind(gameRanks, strategies[, topWin]))
    }
    data
  }, drop = FALSE)

})
