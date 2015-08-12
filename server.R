rm(list = ls())
library(shiny)
load("useWeeklyFile.RData")

# Define a server for the Shiny app
shinyServer(function(input, output) { # input <- data.frame(players = 250, first = 225, second = 125, third = 50)

  reactive({results <- littleSim(numFans = input$players)})
  #reactive({simulatePool(numFans = input$players, payouts = c(input$first, input$second, input$third))})
  reactive({winnings <- round(as.data.frame(t((results %*% c(input$first, input$second, input$third)))), 1)})
  reactive({inTheMoney <- round(rowSums(results %*% (1*(c(input$first, input$second, input$third) > 0))), 2)})

  reactive({colnames(winnings) <- c("WTP", "Fav", "Fav-1", "Fav-2", "Fav-3", "Fav-4",
                           "Fav-5", "Fav-6", "Fav-7", "Fav-8", "Fav-9",
                           "Fav-10", "Fav-11", "Fav-12")})
  reactive({rownames(results) <- colnames(winnings)})

  reactive({topWin <- order(-winnings[1,])[1:3]})
  reactive({topMoney <- order(-inTheMoney)[1:3]})

  # Filter data based on selections
  output$resultsTable <- renderTable({

    data <- as.data.frame(cbind(gameRanks, favorites, strategies[, topWin()], strategies[, topMoney()]))
    colnames(data) <- c("Confidence", "Favorites", "Top $", "2nd $", "3rd $", "Most Top3", "2nd Top3", "3rd Top3")
    data
  }, drop = FALSE)

})
