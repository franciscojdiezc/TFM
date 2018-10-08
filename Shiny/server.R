library(shiny)

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
    
    table1$Prediccion = predict.glm(modeloLogit,type="response", newdata = table1)*100
    
    output$text <- renderText({pred})
   
  })

  values <- reactiveValues()
  
  values$df <- data.frame('Div' ="", 'Mes'="", 'Probabilidad'="", 'LocalVisitante'="", 'JuegaEuropa'="",
                          'MundialOEurocopa'="", 'Prediccion'="")
    
    observe({
      
      Div <- as.character(input$Div)
      Mes <- as.integer(input$Mes)
      Probabilidad <- as.numeric(input$Probabilidad)
      LocalVisitante <- as.integer(input$LocalVisitante)
      JuegaEuropa <- as.integer(input$JuegaEuropa)
      MundialOEurocopa <- as.integer(input$MundialOEurocopa)
      
    if(input$Predecir > 0) {
      
      table2 <- cbind(Div, Mes, Probabilidad, LocalVisitante, JuegaEuropa, MundialOEurocopa)
      table2 <- as.data.frame(table2,stringsAsFactors = default.stringsAsFactors())
      table2$Probabilidad <- as.numeric(levels(table2$Probabilidad))[table2$Probabilidad]
      table2$Prediccion <- predict.glm(modeloLogit,type="response", newdata = table2)*100
      
      table2$Probabilidad <- as.character(table2$Probabilidad)
      table2$Prediccion <- as.character(table2$Prediccion)
      
      isolate(values$df <- rbind(as.matrix(values$df), table2))
      }
    
    output$table <- renderTable({values$df}, include.rownames=F)
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("predicciones",".csv", sep="")
      },
      content = function(file) {
        write.csv(values$df, file)
      })
      contentType = "text/csv"

      })
  })
