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
  
  observe({              
    Div <- as.character(input$Div)
    Mes <- as.integer(input$Mes)
    Probabilidad <- as.numeric(input$Probabilidad)
    LocalVisitante <- as.integer(input$LocalVisitante)
    JuegaEuropa <- as.integer(input$JuegaEuropa)
    MundialOEurocopa <- as.integer(input$MundialOEurocopa)

    table1 <- cbind(Div, Mes, Probabilidad, LocalVisitante, JuegaEuropa, MundialOEurocopa)
    table1 <- as.data.frame(table1,stringsAsFactors = default.stringsAsFactors())
    table1$Probabilidad <- as.numeric(levels(table1$Probabilidad))[table1$Probabilidad]
    
    pred = predict.glm(modeloLogit,type="response", newdata = table1)*100
    
    output$table <- renderTable({table1})
    
    output$text <- renderText({pred})
    
  })
    
  })
