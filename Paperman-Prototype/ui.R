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
      textInput("history1", "Enter an article Id:",
                placeholder = "http://arxiv.org/abs/1711.06826v1"),
      textInput("history2", "Enter an article Id:",
                placeholder = 'http://arxiv.org/abs/1602.08644v3'),
      textInput("history3", "Enter an article Id:",
                placeholder = 'http://arxiv.org/abs/1708.08275v1'),
      textInput("history4", "Enter an article Id:",
                placeholder = 'http://arxiv.org/abs/1707.04860v1'),
      textInput("history5", "Enter an article Id:",
                placeholder = 'http://arxiv.org/abs/1212.4777v1'),
      
      # Find similar documents
      actionButton(inputId = "go_button",
                   "Find Me Papers!")
      
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
