library(shiny)

load("fsims2.RData")

# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  # Filter data based on selections
  output$table <- renderDataTable({
    if (input$select == "Top Payouts"){
      data <- strategies[, topWin]
    } else if (input$select == "Top Winners"){
      data <- strategies[, topMoney]
    } else {
      data <- favorites
    }
    data
  })
  
})
