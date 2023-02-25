# lintr:disable-all
# Charger les packages nécessaires

library(shiny)
library(ggplot2)
library(rsconnect)

# rsconnect::deployApp(appDir = "C:/Users/alexa/Downloads/Git/Dataviz/TPs/TP5/DashIrisTP5", appName = "DashIrisTP5")


# Charger le fichier iris.data
iris_data <- read.csv("iris.data", header=FALSE)

# Création de l'interface utilisateur
ui <- fluidPage(
  titlePanel("Iris Dataset Visualizations"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "x_var",
                  label = "Choose x-axis variable",
                  choices = c("sepal_length", "sepal_width", "petal_length", "petal_width"),
                  selected = "sepal_width"),
      selectInput(inputId = "y_var",
                  label = "Choose y-axis variable",
                  choices = c("sepal_length", "sepal_width", "petal_length", "petal_width"),
                  selected = "sepal_length"),
      checkboxInput(inputId = "jitter",
                    label = "Add jitter",
                    value = FALSE)
    ),
    mainPanel(
      plotOutput(outputId = "histogram"),
      plotOutput(outputId = "density"),
      plotOutput(outputId = "boxplot"),
      plotOutput(outputId = "violin"),
      plotOutput(outputId = "scatterplot")
    )
  )
)

# Création du serveur
server <- function(input, output) {
  
  # Chargement des données
  iris_data <- read.csv("iris.data")
  
  # Histogramme
  output$histogram <- renderPlot({
    ggplot(iris_data, aes_string(x='sepal_width', fill='class')) + geom_histogram(binwidth = 0.2)
  })
  
  # Densité
  output$density <- renderPlot({
    ggplot(iris_data, aes_string(x=input$x_var, fill='class')) +
      geom_density(alpha=0.4)
  })
  
  # Boxplot
  output$boxplot <- renderPlot({
    ggplot(iris_data, aes_string(x = 'class', y=input$y_var)) +
      geom_boxplot() +
      geom_point(aes(colour = class), alpha=0.2, position = ifelse(input$jitter, "jitter", "identity"))
  })
  
  # Violin plot
  output$violin <- renderPlot({
    ggplot(iris_data, aes_string(x = 'class', y=input$y_var)) +
      geom_violin(aes(fill=class)) +
      geom_point(aes(colour = class),
                 alpha=0.2, position = ifelse(input$jitter, "jitter", "identity"))
  })
  
  # Scatterplot
  output$scatterplot <- renderPlot({
    ggplot(iris_data, aes_string(x=input$x_var, y = input$y_var)) +
      geom_point(aes(colour = class), position = ifelse(input$jitter, "jitter", "identity")) +
      geom_smooth(method="lm", se=FALSE) +
      facet_wrap(~class)
  })
}

# Lancement de l'application Shiny
shinyApp(ui = ui, server = server)
