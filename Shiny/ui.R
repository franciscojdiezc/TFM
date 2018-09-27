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
  sidebarLayout(
    sidebarPanel(
      selectInput("Div", "Liga:", c("P1" , "SP1" , "I1" , "D1" , "E0" , "F1"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput('table')
    )
  )
))
