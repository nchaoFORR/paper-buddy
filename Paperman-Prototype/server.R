#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

source('scripts/functions/text-functions.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
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

  ### load metadata
  doc_meta <- read_rds('data/doc_meta_medium.rds') %>% distinct()

  choices <- doc_meta %>% filter(var == "title") %>% .$val 

  updateSelectizeInput(session, 'history_selector',
                       choices = choices, server = TRUE)



  ### run pipeline and render most similar docs on observe button
  observeEvent(input$go_button, {
    
    message("fetching similar documents...")
    
    selected_embedding <- input$embedding_selector
    
    if(selected_embedding == "Custom") {
      emb <- embeddings$custom
    } else if(selected_embedding == "PCA-Combined") {
      emb <- embeddings$pca
    } else if(selected_embedding == "SVD-Combined") {
      emb <- embeddings$svd
    }
    
    word_vecs <- emb$word
    doc_vecs <- emb$doc
    
    # get selected id's
    article_lookup <-
      doc_meta %>% 
      filter(var %in% c("article_id", 'title')) %>% 
      spread('var', 'val')
    
    # grab doc vecs
    selected_doc_vecs <-
      article_lookup %>% 
      filter(title %in% input$history_selector) %>% 
      .$article_id
    
    # combine docs
    combined_doc_vecs <- combine_docs(selected_doc_vecs, doc_vecs)
    
    # grab similar docs
    out <- get_similar_docs(doc_vec = combined_doc_vecs,
                                dictionary = doc_vecs,
                                meta = doc_meta,
                                docs_used = selected_doc_vecs,
                                how_many = 6)
    
    message('found similar documents')
    
    output$result_table <- renderTable({
      out
    },
    hover = TRUE,
    bordered = TRUE,
    spacing = "s",
    align = 'l',
    digits = 2)
    
  })

  ### Output table


  

  ### calcuate document vector and find similar documents in reactive
  
  
})
