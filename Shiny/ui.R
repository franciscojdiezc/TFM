#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  tags$head(
    tags$meta(charset="utf-8"),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
    tags$link(rel = "shortcut icon", type = "image/png", href = "favicon.png"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css", media="all")
  ),
 
  tags$body(
    tags$script(src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.2/jquery.min.js"),
    tags$script(src="main.js")
    
  ),

  # Application title
  titlePanel("", windowTitle = "BettingFav"),
  
    sidebarPanel(
      
      h2("Instrucciones"),
      p("En los partidos donde hay un equipo favorito, selecciona:"),
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
      
      tabsetPanel(type = 'tabs',
                  
        tabPanel("Probabilidad",
          h1("La probabilidad de que gane el favorito es:"),
          h2(textOutput('text'),
          h3("Compartir en redes sociales"),
           tags$div(class="shared-panel",
           tags$a(target="_blank", class="shared fb st3",title="Facebook"),
           tags$a(target='_blank', class="shared tt st3",title="Twitter"),
           tags$a(target='_blank', class="shared gp st3",title="Google+"),
           tags$a(target='_blank', class="shared wa st3",title="WhatsApp"),
           tags$a(target='_blank', class="shared ce st3", title="Email")))),
          
        tabPanel("Resumen",
          tags$table(tableOutput("table")),
          downloadButton("downloadData", "Download")))
)
)
)