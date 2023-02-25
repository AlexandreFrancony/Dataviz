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

ui <- fluidPage(
sidebarLayout(
  sidebarPanel(
    numericInput(inputId = "row_number",
                 label = "Please, select number of row to display:",
                 min=1,
                 max=100,
                 value=5),
      br(),
      br(),
      br(),
      br(),
    sliderInput(inputId = "bins",
                "Number of bins:",
                min = 1,
                max = 50,
                value = 20),
    br(),
    br(),
    br(),
    br(),
    selectInput(inputId = "colors",
                label = "Please, select a color:",
                choices  = c("red","blue","green"),  #choices = colours()
                selected ="blue")

  ),
  mainPanel( h2('Exploration des donnees faithful'),
             tabsetPanel(type = "pills",
               tabPanel('Echantillon de donnees', tableOutput(outputId = "tab")),
               tabPanel('Distribution', plotOutput(outputId = "distPlot")),
               tabPanel('Nuage des points', plotOutput(outputId = "distPlot1"))
             )
  )
)
)
######################


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

