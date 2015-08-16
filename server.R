rm(list = ls())
library(shiny); library(scales)
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

  output$exp1 <- renderText({

    sapply(winDollars()[1,order(-winDollars())], dollar)[1]
  })

  output$exp2 <- renderText({

    sapply(winDollars()[1,order(-winDollars())], dollar)[2]
#    as.character(paste0("$", winDollars()[order(-winDollars())][2], sep = ""))
  })

  output$exp3 <- renderText({

    sapply(winDollars()[1,order(-winDollars())], dollar)[3]
#    as.character(paste0("$", winDollars()[order(-winDollars())][3], sep = ""))
  })

  output$expFav <- renderText({
    sapply(winDollars()[1,], dollar)[1]
    # as.character(paste0("$", winDollars()[1], sep = ""))
  })

  output$expSlate1 <- renderTable({

    #data3 <- as.data.frame(t(c(NULL,winDollars()[1], winDollars()[order(-winDollars())][1:3])))
    data1 <- as.data.frame(cbind(gameRanks, strategies[, order(-winDollars())][, 1]))

    colnames(data1) <- c("Confidence", "Slate 1")
    data1
  })

  output$expSlate2 <- renderTable({

    #data3 <- as.data.frame(t(c(NULL,winDollars()[1], winDollars()[order(-winDollars())][1:3])))
    data2 <- as.data.frame(cbind(gameRanks, strategies[, order(-winDollars())][, 2]))

    colnames(data2) <- c("Confidence", "Slate 2")
    data2
  })

  output$expSlate3 <- renderTable({

    #data3 <- as.data.frame(t(c(NULL,winDollars()[1], winDollars()[order(-winDollars())][1:3])))
    data3 <- as.data.frame(cbind(gameRanks, strategies[, order(-winDollars())][, 3]))

    colnames(data3) <- c("Confidence", "Slate 3")
    data3
  })

  output$expSlateF <- renderTable({

    #data3 <- as.data.frame(t(c(NULL,winDollars()[1], winDollars()[order(-winDollars())][1:3])))
    dataF <- as.data.frame(cbind(gameRanks, favorites))

    colnames(dataF) <- c("Confidence","Favorites")
    dataF
  })

#   output$expected <- renderTable({
#
#     data3 <- as.data.frame(cbind(gameRanks,winDollars()[1])) #, winDollars()[order(-winDollars())][1:3])))
#     #data <- as.data.frame(cbind(gameRanks, favorites))
#
#     colnames(data3) <- c("Confidence","Favorites")
#     data3
#   })

  output$ITM <- renderTable({

    dataI1 <- as.data.frame(cbind(gameRanks, favorites, strategies[,order(-inTheMoney())[1:3]]))
    # data <- as.data.frame(cbind(gameRanks, favorites, topWin, topMoney))

    colnames(dataI1) <- c("Confidence", "Favorites", "Most Frequent", "2nd", "3rd")
    dataI1
  })

  output$freq <- renderTable({

    dataF <- as.data.frame(t(c(NULL,inTheMoney()[1], inTheMoney()[order(-inTheMoney())][1:3])))
    # data <- as.data.frame(cbind(gameRanks, favorites, topWin, topMoney))

    colnames(dataF) <- c("Favorites", "Most Frequent","2nd","3rd")
    dataF
  })

})
