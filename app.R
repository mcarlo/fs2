library(xtable)
library(shiny)

shiny::runApp(
  list(
    ui = fluidPage(
      titlePanel("WinThatPool"),
      
      # Create a new Row in the UI for selectInputs
      fluidRow(
        column(3, 
               numericInput("players", "Number of entries in pool:", 100)
        )
      ),
      fluidRow(
        column(4, 
               numericInput("first", "First place payout:", 225)
        ),
        column(4, 
               numericInput("second", "Second place payout:", 125)
        ),
        column(4, 
               numericInput("third", "Third place payout:", 50)
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
      simulatePool()
      
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