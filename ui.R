library(shiny)

# Define the overall UI
shinyUI(
  fluidPage(
    titlePanel("WinThatPool"),

    # Create a new Row in the UI for selectInputs
    fluidRow(
      column(3,
             numericInput("players", "Number of entries in pool:", 100)
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
    # Create a new row for the table.
      tableOutput(outputId="table")
    )
  )
)
