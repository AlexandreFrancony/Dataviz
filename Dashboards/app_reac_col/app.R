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
  fluidRow(
    column(4,numericInput(inputId = "row_number",
                          label = "Please, select number of row to display:",
                          min=1,
                          max=100,
                          value=5)),
    column(4,sliderInput(inputId = "bins",
                         "Number of bins:",
                         min = 1,
                         max = 50,
                         value = 20)),
    column(4,
           selectInput(inputId = "colors",
                       label = "Please, select a color:",
                       choices  = c("red","blue","green"),
                       selected ="blue"))
    
  ),
  fluidRow(
    column(4,dataTableOutput(outputId = "tab")),
    column(4, plotOutput(outputId = "distPlot")),
    column(4,plotOutput(outputId = "distPlot1"))
    
  )
)


############################


server <- function(input, output, session) {
  
  table_filtree <- reactive({
    faithful%>%sample_n(input$row_number)
  })
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    
    x    <- table_filtree()[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = input$colors, border = 'white')
  })
  
  output$distPlot1 <- renderPlot({
    # generate bins based on input$bins from ui.R
    ggplot(table_filtree(), aes(x =eruptions , y=waiting ))+
      geom_point(col = input$colors)
  })
  
  output$tab <- renderDataTable({ table_filtree()#head(faithful, input$row_number)
    
  })   
}

# Run the application 
shinyApp(ui = ui, server = server)

