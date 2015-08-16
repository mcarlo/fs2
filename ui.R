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

    # Create a new row for the table.
    fluidRow(
      tabsetPanel(
        tabPanel("Highest Expected Payouts",
                 p("This panel is reactive. Change the Number of Players, and
                   weekly payouts for First, Second, and Third place to see the
                   top three slates change."),

                  h5("Slate 1:", textOutput(outputId = 'exp1', inline = T), " per simulated season."),
                  tableOutput(outputId="expSlate1"),
                  br(),
                  h5("Slate 2:", textOutput(outputId = 'exp2', inline = T), " per simulated season."),
                  tableOutput(outputId="expSlate2"),
                  br(),
                  h5("Slate 3:", textOutput(outputId = 'exp3', inline = T), " per simulated season."),
                  tableOutput(outputId="expSlate3"),
                  br(),
                  h5("Favorites:", textOutput(outputId = 'expFav', inline = T), " per simulated season."),
                  tableOutput(outputId="expSlateF")
                 ),

        tabPanel("Most Often In-the-money",
                 p("This panel is reactive. Change the Number of Players, and
                   weekly payouts for First, Second, and Third place to see the
                   three most frequently winning slates change."),

                 h5("Slate 1: In the money ", textOutput(outputId = 'freq1', inline = T), " weeks per simulated season."),
                 tableOutput(outputId="ITM1"),
                 br(),
                 h5("Slate 2: In the money ", textOutput(outputId = 'freq2', inline = T), " weeks per simulated season."),
                 tableOutput(outputId="ITM2"),
                 br(),
                 h5("Slate 3: In the money ", textOutput(outputId = 'freq3', inline = T), " weeks per simulated season."),
                 tableOutput(outputId="ITM3"),
                 br(),
                 h5("Favorites: In the money ", textOutput(outputId = 'freqFav', inline = T), " weeks per simulated season."),
                 tableOutput(outputId="ITMF"),
                 br()
#                  ,
#                  tableOutput(outputId="freq"),
#                  tableOutput(outputId="ITM"))
    )
  )
)
))
