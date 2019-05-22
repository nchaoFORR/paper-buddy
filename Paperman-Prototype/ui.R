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
  titlePanel("Paper Buddy"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      # Select the embedding
      selectInput(inputId = "embedding_selector",
                  label = "Select an embedding",
                  choices = c("Custom", "PCA-Combined", "SVD-Combined"),
                  selected = "PCA-Combined"),
      
      # Enter articles you've read
      selectizeInput(inputId = "history_selector",
                     label = "Enter papers you've recently read",
                     choices = NULL,
                     multiple = TRUE),
      
      # Find similar documents
      actionButton(inputId = "go_button",
                   "Find Me Papers!")
      
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
       
      # Main Results Table
      uiOutput(outputId = "result_table")
      
    )
  )
))
