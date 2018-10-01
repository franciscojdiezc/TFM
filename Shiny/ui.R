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
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
  ),
    
  # Application title
  titlePanel("Título"),
  
    sidebarPanel(
      selectInput("Div", "Liga:", c("Portugal" = "P1" , "España" = "SP1" ,
                                    "Italia" = "I1" , "Alemania" = "D1" ,
                                    "Inglaterra" = "E0" , "Francia" = "F1")),
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
    
    mainPanel(
      h1("La probabilidad de que gane el favorito es:"),textOutput('text'),
      img(src="foto.jpg", width="100%", height= "450")
    )
  )
)
