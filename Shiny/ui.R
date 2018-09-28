#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Precide si ganará el equipo favorito"),
  
  # Sidebar with a slider input for number of bins 
    sidebarPanel(
      selectInput("Div", "Liga:", c("P1" , "SP1" , "I1" , "D1" , "E0" , "F1")),
      radioButtons("LocalVisitante", "Local:", 
                   choices = list("Local" = 1, "Visitante" = 0)),
      radioButtons("JuegaEuropa", "Juega Europa", 
                   choices = list("Sí" = 1, "No" = 0)),
      radioButtons("MundialOEurocopa", "¿Es año de Mundial o Eurocopa?", 
                   choices = list("Sí" = 1, "No" = 0)),
      sliderInput("Mes", "Mes:",
                  min = 1, max = 12, value = 1),
      numericInput("Probabilidad", "Cuota", value = 1.2,
                   min = 1.01, max = 100, step = 0.01),
      submitButton("Predecir", icon("refresh"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput('table'),
      textOutput('text')
    )
  )
)
