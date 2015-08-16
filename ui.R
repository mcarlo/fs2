library(shiny)

# Define the overall UI
shinyUI(
  fluidPage(
    titlePanel("WinThatPool"),

    # Create a new Row in the UI for selectInputs
    fluidRow(
      column(3,
             sliderInput("players", label = "Number of Players in Pool:", min = 10, 
                         max = 250, step = 10, value = 100)
      )
    ),
    fluidRow(
        column(3,
             sliderInput("first", "First place payout:", min = 10, 
                         max = 250, step = 5, value = 100)
      ),
      column(3,
             sliderInput("second", "Second place payout:", min = 0, 
                         max = 250, step = 5, value = 50)
      ),
      column(3,
             sliderInput("third", "Third place payout:", min = 0, 
                         max = 250, step = 5, value = 25)
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
