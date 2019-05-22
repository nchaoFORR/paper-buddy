#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

source('scripts/functions/text-functions.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  ### load embeddings into lists
  embeddings <- list(
    custom = list(
      word = read_rds('data/word-embeddings/custom-glove-embeddings.rds'),
      doc = read_rds('data/glove_doc_embeddings.rds')
    ),
    pca = list(
      word = read_rds('data/word-embeddings/concat_pca_tibble.rds'),
      doc = read_rds('data/pca-doc-embedding.rds')
    ),
    svd = list(
      word = read_rds('data/word-embeddings/concat_svd_tibble.rds'),
      doc = read_rds('data/svd-doc-embedding.rds')
    )
  )
  
  ### fire when go button pressed
  observeEvent(input$go_button, {
    
    ### create docs into string
    
    
  })
  
  
})
