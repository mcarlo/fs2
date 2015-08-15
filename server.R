rm(list = ls())
library(shiny)
load("useWeeklyFile.RData")

# Define a server for the Shiny app
shinyServer(function(input, output) { # input <- data.frame(players = 250, first = 225, second = 125, third = 50)

  results <- reactive({calcWinners(input$players)})

  winDollars <- reactive({round(as.data.frame(t((results() %*% c(input$first, input$second, input$third)))), 1)})

  inTheMoney <- reactive({round(rowSums(results() %*% (1*(c(input$first, input$second, input$third) > 0))), 2)})

  output$Winnings <- renderTable({

    data <- as.data.frame(cbind(gameRanks, favorites, strategies[,order(-winDollars()[1,])[1:3]]))
    # data <- as.data.frame(cbind(gameRanks, favorites, topWin, topMoney))
    
    colnames(data) <- c("Confidence", "$ Favorites", "$1st","$2nd","$3rd")
    data
  })
  
  output$expected <- renderTable({
    
    data3 <- as.data.frame(t(c(NULL,winDollars()[1], winDollars()[order(-winDollars())][1:3])))
    # data <- as.data.frame(cbind(gameRanks, favorites, topWin, topMoney))
    
    colnames(data3) <- c("$ Favorites", "$1st","$2nd","$3rd")
    data3
  })
  
  output$ITM <- renderTable({

    data1 <- as.data.frame(cbind(gameRanks, favorites, strategies[,order(-inTheMoney())[1:3]]))
    # data <- as.data.frame(cbind(gameRanks, favorites, topWin, topMoney))
    
    colnames(data1) <- c("Confidence", "Favorites", "Most Frequent", "2nd", "3rd")
    data1
  })
  
  output$freq <- renderTable({
    
    data2 <- as.data.frame(t(c(NULL,inTheMoney()[1], inTheMoney()[order(-inTheMoney())][1:3])))
    # data <- as.data.frame(cbind(gameRanks, favorites, topWin, topMoney))
    
    colnames(data2) <- c("Favorites", "Most Frequent","2nd","3rd")
    data2
  })
  
})
