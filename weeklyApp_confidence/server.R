#rm(list = ls())
library(shiny); library(scales)
require(googleVis)
#setwd("C:/Users/Anichini/Documents/GitHub/fs2") #
#setwd("D:/Documents/GitHub/fs2")
load("useWeeklyFile2015_04.RData")
nGames <- length(gameRanks)

favorites <- weekFileConf$Victor

# Define a server for the Shiny app
shinyServer(function(input, output) { # input <- data.frame(players = 25, first = 100, second = 0, third = 0)

  results <- reactive({resultsLists[[2 * input$players/5]]})
  #results <- reactive({calcWinners(input$players)})
  #results <- calcWinners(input$players)

  winDollars <- reactive({round(as.data.frame(t((results() %*% c(input$first, input$second, input$third)))), 1)})
  # winDollars <- round(as.data.frame(t((results %*% c(input$first, input$second, input$third)))), 1)

  inTheMoney <- reactive({round(rowSums(results() %*% (1*(c(input$first, input$second, input$third) > 0))), 2)})
  # inTheMoney <- round(rowSums(results %*% (1*(c(input$first, input$second, input$third) > 0))), 2)

  output$Winnings <- renderTable({

    data <- as.data.frame(cbind(gameRanks, favorites, strategies[,order(-winDollars()[1,])[1:3]]))
    # data <- as.data.frame(cbind(gameRanks, favorites, strategies[,order(-winDollars[1,])[1:3]]))

    colnames(data) <- c("Confidence", "$ Favorites", "$1st","$2nd","$3rd")
    data
  })

  output$expSlate1 <- renderTable({
    data1 <- as.data.frame(cbind(gameRanks, strategies[, order(-winDollars())][, 1]))

    colnames(data1) <- c("Confidence", "Pick")
    data1
  }, include.rownames = F)


  output$gSlate1 <- renderGvis({
    data1 <- as.data.frame(cbind(gameRanks, strategies[, order(-winDollars())][, 1]))

    colnames(data1) <- c("Confidence", "Pick")
    maxPoints <- gvisTable(data1,
                           options=list(page='enable', #height=500, width = 300,
                                      showRowNumber = F, pageSize = nGames,
                                        cssClassNames = "{headerRow: 'myTableHeadrow',
                                        tableRow: 'myTablerow'}",
                                        alternatingRowStyle = TRUE, page = 'disable'),
                           chartid = "maxPointsTable")
    maxPoints

  })

  output$ITM1 <- renderTable({

    dataI1 <- as.data.frame(cbind(gameRanks, strategies[,order(-inTheMoney())[1]]))

    colnames(dataI1) <- c("Confidence", "Pick")
    dataI1
  }, include.rownames = F)

  output$gITM1 <- renderGvis({
    dataI1 <- as.data.frame(cbind(gameRanks, strategies[,order(-inTheMoney())[1]]))

    colnames(dataI1) <- c("Confidence", "Pick")
    mostFreq <- gvisTable(dataI1,
                           options=list(page='enable', #height=500, width = 300,
                                        showRowNumber = F, pageSize = nGames,
                                        cssClassNames = "{headerRow: 'myTableHeadrow',
                                        tableRow: 'myTablerow'}",
                                        alternatingRowStyle = TRUE, page = 'disable'),
                           chartid = "mostFreqTable")
    mostFreq

})

})
