library(shiny)

# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)

# Define the overall UI
shinyUI(
  fluidPage(
    titlePanel("WinThatPool!"),
          
    # Create a new Row in the UI for selectInputs
    fluidRow(
      column(4, 
             numericInput("players", "Number of entries in pool:", 200),
      ),
      column(4, 
             numericInput("first", "First place payout:", 220),
      ),
      column(4, 
             numericInput("second", "Second place payout:", 125),
      ),
      column(4, 
             numericInput("third", "Third place payout:", 50),
      )
    ),
    # Create a new row for the table.
    fluidRow(
      # an ordinary selectize input without option groups
      selectizeInput('display', 'Select', choices = c("Favorites", "Top Payouts", "Top Winners"))
    ),    
    # Create a new row for the table.
    fluidRow(
      dataTableOutput(outputId="table")
    )    
  )  
)
