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
       column(6, numericInput("players", label = NA, min = 10,
                              max = 250, step = 10, value = 100)
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
      column(4,
             h4("Highest simulated payout"),
             tableOutput(outputId="expSlate1")
      ),
      column(6,
             p("These picks averaged the highest payouts in WinThatPool!'s 
simulations of this week's outcomes."),
             br(),
             p("Use the controls to change the Number of Players, and the
                   weekly payouts for First, Second, and Third place to see if the
               picks change.")
             )),
    fluidRow(
      column(4,
             h4("Second highest payout"),
             tableOutput(outputId="expSlate2")
      ),
      column(6,
             p("These picks averaged the second highest payouts in WinThatPool!'s 
               simulations of this week's outcomes."),
             br(),
             p("Use the controls to change the Number of Players, and the
               weekly payouts for First, Second, and Third place to see if the
               picks change.")
             )),
    fluidRow(
      column(4,
             h4("Third highest payout"),
             tableOutput(outputId="expSlate3")
      ),
      column(6,
             p("These picks averaged the third highest payouts in WinThatPool!'s 
               simulations of this week's outcomes."),
             br(),
             p("Use the controls to change the Number of Players, and the
               weekly payouts for First, Second, and Third place to see if the
               picks change.")
             )),
    fluidRow(
      column(4,
             h4("Chalk
                "),
             tableOutput(outputId="expSlateF")
      ),
      column(6,
             p("These are WinThatPool's recommended picks for a season-long prize. They are listed here for comparison purposes."),
             br(),
             p("The controls won't change these picks.")
             ))
    )
)))