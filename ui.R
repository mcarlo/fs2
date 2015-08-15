library(shiny)

# Define the overall UI
shinyUI(
  fluidPage(
    titlePanel("WinThatPool"),

    # Create a new Row in the UI for selectInputs
    fluidRow(
      column(3,
             numericInput("players", "Number of Players in Pool:", 100)
      )
    ),
    fluidRow(
        column(3,
             numericInput("first", "First place payout:", 225)
      ),
      column(3,
             numericInput("second", "Second place payout:", 125)
      ),
      column(3,
             numericInput("third", "Third place payout:", 50)
      )
    ),
    #fluidRow(actionButton("goButton", "Update and simulate")),
    # Create a new row for the table.
    fluidRow(
      tabsetPanel(
        tabPanel("Highest Expected Payouts", 
                 p("This panel is reactive. Change the Number of Players, and  
                   payouts for First, Second, and Third place to see how the 
                   top expected payout picks change."),                  
                 
                 tableOutput(outputId="expected"),
                 tableOutput(outputId="Winnings")),
        
        tabPanel("Most Often In-the-money", 
                 p("This panel is reactive. Change the Number of Players, and  
                   payouts for First, Second, and Third place to see how the 
                   top picks most likely to win a payout change."),                  
                 
                 tableOutput(outputId="freq"),
                 tableOutput(outputId="ITM"))
        
    # Create a new row for the table.
      # tableOutput(outputId="Winnings")
    )
  )
))
