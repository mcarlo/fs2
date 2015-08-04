library(xtable)
library(shiny)

shiny::runApp(
  list(
    ui = fluidPage(
      titlePanel("WinThatPool"),
      
      # Create a new Row in the UI for selectInputs
      fluidRow(
        column(3, 
               numericInput("players", "Number of entries in pool:", 100, min = 10, max = 250, step = 10)
        )
      ),
      fluidRow(
        column(4, 
               numericInput("first", "First place payout:", 25, max = 500, step = 25)
        ),
        column(4, 
               numericInput("second", "Second place payout:", 10, max = 250, step = 5)
        ),
        column(4, 
               numericInput("third", "Third place payout:", 5, max = 50, step = 5)
        )
      )
#       pageWithSidebar(
#       headerPanel("TEST"),
#       
#       sidebarPanel(
#         helpText('Is this matrix cool ?')
#       )
      ,
      
      mainPanel(    
        uiOutput('matrix')     
      )
    ), 
    server = function(input,output){
      load("useWeeklyFile.RData")
      rm(totalPoints)
      gameRanks <- games:1
      
      myRanks <- rank(winProb, ties.method = "random")+premiumPts
      myPoints <- as.vector(crossprod(myRanks, simOutcomes2)) # * myRanks
      
      stratMatrix <- matrix(cbind(myPoints[resultIndex], upsetPoints[resultIndex,]), nrow = length(resultIndex))
      rankMatrix <- matrix(rep(0, 34000), ncol = 17)
      # input <- data.frame(players = 250, first = 100, second = 0, third = 0)
      reactive({rankMatrix <<- apply(stratMatrix, 2, rankVinM_Q, pointsMtrx = totalPointsIter[, 1:input$players])})
      stratWins <- colSums(rankMatrix[, 1:14] == 1)
      stratPlace <- colSums(rankMatrix[, 1:14] == 2)
      stratShow <- colSums(rankMatrix[, 1:14] == 3)
      payouts <- c(0, 0, 0)
      reactive({payouts <- c(input$first, input$second, input$third)})
      
      resultsMatrix <- as.matrix(cbind(stratWins, stratPlace, stratShow), nrow = 6, ncol = 3) * 17.0 / maxIter
      winnings <- round(as.data.frame(t((resultsMatrix %*% payouts))), 1)
      inTheMoney <- round(rowSums(resultsMatrix %*% (1*(payouts > 0))), 2)
      
      colnames(winnings) <- c("WTP", "Fav", "Fav-1", "Fav-2", "Fav-3", "Fav-4",
                               "Fav-5", "Fav-6", "Fav-7", "Fav-8", "Fav-9",
                               "Fav-10", "Fav-11", "Fav-12")
      rownames(resultsMatrix) <- colnames(winnings)
      #   top3Money()
      #   top3Dollars()
      topWin <- order(-winnings[1,])[1:3]
      topMoney <- order(-inTheMoney)[1:3]
      
      # simulatePool()
      
      output$matrix <- renderUI({
        M <- matrix(c(favorites, strategies[, topWin[1]], strategies[, topMoney[1]]),ncol=3)
        colnames(M) <- c("Payout", "Frequency", "Favorites")
        rownames(M) <- sapply(games:1L, paste0)
        M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
                   floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE, size = "huge")
        html <- paste0("$$", M, "$$")
        list(
          withMathJax(HTML(html))
        )
      })
    }
  )
)
