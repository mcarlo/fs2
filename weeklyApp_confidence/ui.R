library(shiny)
library(googleVis)

# Define the overall UI
shinyUI(fluidPage(
    includeCSS("styles.css"),
  
    # tags$head(includeScript("google-analytics.js")),

    sidebarLayout(
      div(class="set1",sidebarPanel(
        # Create a new Row in the UI for selectInputs
     fluidRow(
       column(1, p("")),
       column(5,
              p("Number of Players in Pool:")),
       column(6, numericInput("players", label = NA, min = 5,
                              max = 100, step = 5, value = 35)
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
      )),

    mainPanel(
  # Create a new row for the table.
      fluidRow(
      column(4,
             h4("Highest simulated payout"),
             htmlOutput(outputId="gSlate1")
             ,
             tags$head(tags$style(type="text/css", 
                                  ".myTableHeadrow {color:#FFFFFF; background-color:#FF0000;} 
                                             .myTablerow {background-color:#D9D9D9;}"))
      ),
      column(4,
             h4("Most often in the money"),
             htmlOutput(outputId="gITM1")
             ,
             tags$head(tags$style(type="text/css", 
                                  ".myTableHeadrow {color:#FFFFFF; background-color:#FF0000;} 
                                  .myTablerow {background-color:#D9D9D9;}"))
             ))
    )
  ),
tags$head(includeScript("google-analytics.js"))

))