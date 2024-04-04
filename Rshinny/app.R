# Charger les bibliothèques nécessaires
install.packages(c("tidyverse", "shiny"))
library(tidyverse)
library(shiny)

# Charger les données depuis le fichier CSV
data <- read.csv("C:/Users/rayan/Downloads/archive/results.csv", sep = ",")

# Convertir la colonne date en format date si elle ne l'est pas déjà
data$date <- as.Date(data$date)

# Définir l'interface utilisateur
ui <- fluidPage(
  titlePanel("Ratio de victoires à domicile et à l'extérieur"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Sélectionnez une équipe :", choices = unique(c(data$home_team, data$away_team)))
    ),
    mainPanel(
      plotOutput("team_plot")
    )
  )
)

# Définir la fonction de calcul et de traçage
server <- function(input, output) {
  # Fonction pour calculer le ratio de victoires à domicile et à l'extérieur pour une équipe donnée
  team_ratio <- reactive({
    team_data <- data %>%
      filter(home_team == input$team | away_team == input$team) %>%
      mutate(
        is_home = ifelse(home_team == input$team, 1, 0),
        is_away = ifelse(away_team == input$team, 1, 0),
        win = ifelse((home_team == input$team & home_score > away_score) | 
                       (away_team == input$team & away_score > home_score), 1, 0)
      )
    home_ratio <- sum(team_data$is_home & team_data$win) / sum(team_data$is_home)
    away_ratio <- sum(team_data$is_away & team_data$win) / sum(team_data$is_away)
    return(c(home_ratio, away_ratio))
  })
  
  # Tracer le graphique
  output$team_plot <- renderPlot({
    ratio <- team_ratio()
    barplot(ratio, names.arg = c("À domicile", "À l'extérieur"), col = c("blue", "green"),
            main = paste("Ratio de victoires pour l'équipe", input$team),
            ylab = "Ratio de victoires", ylim = c(0, 1))
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
