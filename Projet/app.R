# Charger les packages nécessaires
library(shiny)
library(ggplot2)
library(dplyr)

# Charger les données
vgsales <- read.csv("vgsales.csv")

# Supprimer toutes les lignes pour lesquelles l'année est N/A
vgsales <- vgsales %>% filter(!is.na(as.numeric(Year)))

# Conserver uniquement les données pour les consoles souhaitées
vgsales <- vgsales %>% filter(Platform %in% c("3DS", "DS", "NES", "N64", "GBA", "PS", "PS2", "PS3", "PS4", "Wii", "WiiU", "X360", "XB", "XOne"))


# Créer une application Shiny
ui <- fluidPage(
  
  # Définir le titre de l'application
  titlePanel("Ventes de jeux vidéo"),
  
  # Créer une barre latérale avec des filtres pour les données
  sidebarLayout(
    sidebarPanel(
      # Filtre par plateforme
      selectInput("platformInput", "Plateforme:",
                  choices = c("Toutes", unique(vgsales$Platform))),
      # Filtre par genre
      selectInput("genreInput", "Genre:",
                  choices = c("Tous", unique(vgsales$Genre))),
      # Filtre par région
      radioButtons("regionInput", "Région:",
                   choices = c("Toutes", "Amérique du Nord", "Europe", "Japon", "Autres"),
                   selected = "Toutes")
    ),
    # Afficher les graphiques en fonction des filtres
    mainPanel(
      plotOutput("salesPlot"),
      plotOutput("salesByPlatformPlot"),
      plotOutput("salesByGenrePlot")
    )
  )
)

# Définir le serveur Shiny
server <- function(input, output) {
  
  # Filtrer les données en fonction des filtres de la barre latérale
  filteredData <- reactive({
    vgsales %>%
      filter(if (input$platformInput == "Toutes") TRUE else Platform == input$platformInput) %>%
      filter(if (input$genreInput == "Tous") TRUE else Genre == input$genreInput) %>%
      filter(if (input$regionInput == "Toutes") TRUE else
        case_when(input$regionInput == "Amérique du Nord" ~ NA_Sales > 0,
                  input$regionInput == "Europe" ~ EU_Sales > 0,
                  input$regionInput == "Japon" ~ JP_Sales > 0,
                  input$regionInput == "Autres" ~ Other_Sales > 0))
  })
  
  # Afficher le graphique des ventes globales en fonction des filtres
  output$salesPlot <- renderPlot({
    ggplot(filteredData(), aes(x = Year, y = Global_Sales)) +
      geom_line() +
      labs(x = "Année", y = "Ventes globales (en millions)")
  })
  
  # Afficher le graphique des ventes par plateforme en fonction des filtres
  output$salesByPlatformPlot <- renderPlot({
    ggplot(filteredData(), aes(x = Year, y = Global_Sales, color = Platform)) +
      geom_line() +
      labs(x = "Année", y = "Ventes globales (en millions)", color = "Plateforme")
  })
  
  # Afficher le graphique des ventes par genre en fonction des filtres
  output$salesByGenrePlot <- renderPlot({
    ggplot(filteredData(), aes(x = Year, y = Global_Sales, color = Genre)) +
      geom_line() +
      labs(x = "Année", y = "Ventes globales (en millions)", color = "Genre")
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)