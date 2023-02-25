#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(shinythemes)
library(shinydashboard)

library(dplyr)
library(ggplot2)
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
                
                # Application title
                titlePanel("Old Faithful Geyser Data"),
                
                fluidRow(
                  column(3, numericInput(inputId = "row_number",
                                         label = "Please, select number of row to display:",
                                         min=1,
                                         max=100,
                                         value=5) ), 
                  column(5,offset = 4, 
                         tableOutput("tab") )  
                  
                  
                ),
                # Sidebar with a slider input for number of bins 
                fluidRow( 
                  column(3, sliderInput(inputId = "bins",
                                        "Number of bins:",
                                        min = 1,
                                        max = 50,
                                        value = 20)),
                  
                  column(5, offset = 4,plotOutput(outputId = "distPlot")
                         
                  ) ),
                
                fluidRow(
                  column(6,
                         selectInput(inputId = "colors",
                                     label = "Please, select a color:",
                                     choices  = c("red","blue","green"),
                                     selected ="blue")
                  ),
                  column(6,
                         plotOutput(outputId = "distPlot1")
                  )
                )
                
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$tab <- renderTable({
    # generate bins based on input$bins from ui.R
    table_filtree <- faithful%>%sample_n(input$row_number)})
  
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  output$distPlot1 <- renderPlot({
    # generate bins based on input$bins from ui.R
    ggplot(faithful, aes(x =eruptions , y=waiting ))+
      geom_point(col = input$colors)
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)

