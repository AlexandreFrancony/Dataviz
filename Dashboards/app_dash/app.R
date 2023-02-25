#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#Les donnees dans le fichier faithful representent les delais entre eruptions et les
# durees des eruptions d'un geyser dans le parc Yellowstone aux USA. Nous allons examiner
# la distribution des ces donnees a l'aide des graphiques et des statistiques de base. C'est une
# etape prealable obligatoire avant toute analyse et modelisation statistique.

library(shiny)
library(shinythemes)
library(shinydashboard)

library(dplyr)
library(ggplot2)
# Define UI for application that draws a histogram

#######################
ui <- dashboardPage(
  dashboardHeader(title = "My Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(title = "Taille de l'echantillon", solidHeader = TRUE, background = "black",
          numericInput(inputId = "row_number",
                       label = "Please, select number of row to display:",
                       min=1,
                       max=100,
                       value=5)
      ),
      box(title = "Echantillon", solidHeader = TRUE,status = "primary",
          tableOutput(outputId = "tab")
      )
    ),
    br(),
    br(),
    br(),
    br(),
      fluidRow(
        box(title = "Bins", solidHeader = TRUE, background = "black", sliderInput(inputId = "bins",
                                    "Number of bins:",
                                    min = 1,
                                    max = 50,
                                    value = 20)
        ),
        box(title = "Histogram", solidHeader = TRUE,status = "warning",plotOutput(outputId = "distPlot")
        )
      ),
      br(),
      br(),
      br(),
      br(),
      fluidRow(
        box(title = "Coleur du nuage des points", solidHeader = TRUE, background = "black",
               selectInput(inputId = "colors",
                           label = "Please, select a color:",
                           choices  = c("red","blue","green"),
                           selected ="blue")
        ),
        box(title = "Nuage des points", solidHeader = TRUE,background = "maroon",
               plotOutput(outputId = "distPlot1")
        )
      )


  )
)



####################
# Define server logic required to draw a histogram
server <- function(input, output, session) {

   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      bins <- seq(min(x), max(x), length.out = input$bins + 1)

      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })

   output$distPlot1 <- renderPlot({
     # generate bins based on input$bins from ui.R
     ggplot(faithful, aes(x =eruptions , y=waiting ))+
       geom_point(col = input$colors)
   })

   output$tab <- renderTable({head(faithful, input$row_number)

   })
}

# Run the application 
shinyApp(ui = ui, server = server)

