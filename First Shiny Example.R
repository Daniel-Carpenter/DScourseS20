# Tutorial from: https://mastering-shiny.org/basic-app.html
  
library(shiny)
library(tidyverse)
library(datasets)
library(ggthemes)

df <- mtcars

# INPUT: Create the User Interface and Inputs -------------------------------------------------
  ui <- fluidPage(
      h1("Shiny App Example Using 'mtcars' Dataset"),
      sidebarLayout(
        
          ## Side Bar
            sidebarPanel(
              ### DV: X Variable Inputs
                selectInput("x", "Choose an input variable", 
                            choices = names(df), 
                            selected = "wt"),
                
              ### DV: Y Variable Inputs
                selectInput("y", "Choose an outcome variable", 
                            choices = names(df), 
                            selected = "mpg")),
        
          ## Main Panel
            mainPanel(
              ### Print Scatter Plot from X and Y Variables Abouve to UI
                h2("Variable Relationships"),
                plotOutput("scatter"),
            
              ### Print Summary Statistics to UI
                h2("Summary of Dataset"),
                verbatimTextOutput("summary"))))
      
  
# OUTPUT: Calculations from User Input --------------------------------------------------------
  server <- function(input, output, session)
  {
    ## Render Summary Statistics of Dataset
      output$summary <- renderPrint(
      {
        summary(df)
      })
    
    ## Render Scatter Plot
      output$scatter <- renderCachedPlot(
        {
          ggplot(df, 
                 aes(.data[[input$x]], 
                     .data[[input$y]])) + 
            geom_point() +
            geom_smooth() +
            theme_minimal()
        },
        cacheKeyExpr = list(input$x, input$y))
    
    ## Render Exact Dastaset
      output$table <- renderTable(
      {
        print(df)
      })
  }

  
# LAUNCH APP -----------------------------------------------------------------------------------
  shinyApp(ui, server)
  
  
  