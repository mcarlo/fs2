library(shiny)

# Define the overall UI
shinyUI(fluidPage(
    titlePanel("WinThatPool"),

    # tags$head(includeScript("google-analytics.js")),

    sidebarLayout(
      sidebarPanel(
        # Create a new Row in the UI for selectInputs
     fluidRow(
       column(1, p("")),
       column(5,
              p("Number of Players in Pool:")),
       column(6, numericInput("players", label = NA, min = 5,
                              max = 250, step = 5, value = 100)
              )

       # )
     ),
     fluidRow(
#         column(3,
       sliderInput("first", "First place payout:", min = 10,
                         max = 250, step = 5, value = 100),
#       ),
#       column(3,
       sliderInput("second", "Second place payout:", min = 0,
                         max = 250, step = 5, value = 50),
#       ),
#       column(3,
             sliderInput("third", "Third place payout:", min = 0,
                         max = 250, step = 5, value = 25)
       )
      ),

    mainPanel(
  # Create a new row for the table.
      fluidRow(
      column(6,
             h4("Highest simulated payout"),
             tableOutput(outputId="expSlate1")
             ),
      column(6,
             h4("Most often in the money"),
             tableOutput(outputId="ITM1")



      ))
    )
  )
))