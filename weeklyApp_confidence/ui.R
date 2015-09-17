library(shiny)
library(googleVis)


# Define the overall UI
shinyUI(fluidPage(
  includeCSS("styles.css"),
  fluidRow(
    column(3, p("")),
    column(9,
           h3("Tactical picks for 2015"), #style = "color: #06018f;"),
           h6("Appropriate for confidence pools with a weekly payout"),
           sliderInput("players", "   Number of Players in Pool:", min = 5,
                       max = 100, step = 5, value = 35),
           p("Weekly payout:"),
           numericInput("first", "1st", min = 10,
                        max = 250, step = 5, value = 100),
           numericInput("second", "2nd", min = 0,
                        max = 250, step = 5, value = 50),
           numericInput("third", "3rd", min = 0,
                        max = 250, step = 5, value = 25),


           h4("Highest average payoff, Week 2"),
           p("Final update 9/17/16 8:30a PDT"),
           htmlOutput(outputId="gSlate1")
           ,
           tags$head(tags$style(type="text/css",
                                ".myTableHeadrow {color:#FFFFFF; background-color:#FF0000;}
                                .myTablerow {background-color:#D9D9D9;}")),
           h4("Most often in the money"),
           htmlOutput(outputId="gITM1")
           ,
           tags$head(tags$style(type="text/css",
                                ".myTableHeadrow {color:#FFFFFF; background-color:#FF0000;}
                                .myTablerow {background-color:#D9D9D9;}"))
           )
  ),
  tags$head(includeScript("google-analytics.js"))
)

)

# Define the overall UI
# shinyUI(fluidPage(
#     includeCSS("styles.css"),
#
#     # tags$head(includeScript("google-analytics.js")),
#
#     sidebarLayout(
#       div(class="set1",sidebarPanel(
#         # Create a new Row in the UI for selectInputs
#      fluidRow(
#        column(6, p("")),
#        column(6,
#               p("#Players in Pool:"),
#               sliderInput("players", label = NA, min = 5,
#                               max = 100, step = 5, value = 35)
#               )
#
#        # )
#      ),
#      fluidRow(
# #         column(3,
#        column(6, p("")),
#        column(6,
#               p("Weekly payout:"),
#        numericInput("first", "1st", min = 10,
#                          max = 250, step = 5, value = 100),
# #       ),
# #       column(3,
#        numericInput("second", "2nd", min = 0,
#                          max = 250, step = 5, value = 50),
# #       ),
# #       column(3,
#              numericInput("third", "3rd", min = 0,
#                          max = 250, step = 5, value = 25)
#        )
#       ))),
#
#     mainPanel(
#   # Create a new row for the table.
#       fluidRow(
#         column(9, "Final update 9/10/16 9:00a PDT"),
#       column(4,
#              h4("Highest simulated payout"),
#              htmlOutput(outputId="gSlate1")
#              ,
#              tags$head(tags$style(type="text/css",
#                                   ".myTableHeadrow {color:#FFFFFF; background-color:#FF0000;}
#                                              .myTablerow {background-color:#D9D9D9;}"))
#       ),
#       column(4,
#              h4("Most often in the money"),
#              htmlOutput(outputId="gITM1")
#              ,
#              tags$head(tags$style(type="text/css",
#                                   ".myTableHeadrow {color:#FFFFFF; background-color:#FF0000;}
#                                   .myTablerow {background-color:#D9D9D9;}"))
#              )
#       )
#     )
#   ),
#
# tags$head(includeScript("google-analytics.js"))
#
# ))