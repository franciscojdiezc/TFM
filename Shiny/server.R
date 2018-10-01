#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #x <- reactive({data.frame("Div" = input$Div, "Mes" = input$Mes  , 'Probabilidad' = input$Probabilidad,
                  #'LocalVisitante' = input$LocalVisitante,'JuegaEuropa' = input$JuegaEuropa,
                  #'MundialOEurocopa' = input$MundialOEurocopa)})
  observe({              
    Div <- as.character(input$Div)
    Mes <- as.integer(input$Mes)
    Probabilidad <- as.numeric(input$Probabilidad)
    LocalVisitante <- as.integer(input$LocalVisitante)
    JuegaEuropa <- as.integer(input$JuegaEuropa)
    MundialOEurocopa <- as.integer(input$MundialOEurocopa)

    test <- cbind(Div, Mes, Probabilidad, LocalVisitante, JuegaEuropa, MundialOEurocopa)
    test <- as.data.frame(test,stringsAsFactors = default.stringsAsFactors())
    test$Probabilidad <- as.numeric(levels(test$Probabilidad))[test$Probabilidad]
    
    pred = predict.glm(modeloLogit,type="response", newdata = test)*100
    
    output$table <- renderTable({test})
    
    output$text <- renderText({pred})
    
  })
    
  })
