######## Text Analysis Functions

### most_similar()

# this function will return the 10 most similar words.
# word = character vector or a named/unamed word vector (expects tibble format
#        if its a word vector)
# dictionary = word embedding matrix
# how_many = numeric
most_similar <- function(token, dictionary, how_many = 10) {
  
  library(tidyverse)
  
  message(token)
  
  dict_vec <- 
    dictionary %>% 
    select(-word) %>% 
    as.matrix()
  
  
  if(typeof(token) == "character") {
    
  
      tryCatch({
        
        word_vec <-
          dictionary %>% 
          filter(word == token) %>% 
          select(-word) %>% 
          as.matrix()
        
        text2vec::sim2(x = dict_vec,
                       y = word_vec,
                       method = "cosine", norm = "l2") %>% 
          as.data.frame() %>% 
          rename(similarity = V1) %>% 
          mutate(word = dictionary$word) %>% 
          select(word, similarity) %>% 
          filter(word != token) %>% 
          arrange(desc(similarity)) %>% 
          top_n(how_many)
        
        
      },
      error = function(e) {
        
        message('Word not found in dictionary!')
        return()
        
      })
  
  } else {
    
    # check to see if its named or unamed
    if(typeof(token[[1, 1]]) == "character") {
      
      word_vec <- token[,2:ncol(token)] %>% as.matrix()
      
      text2vec::sim2(x = dict_vec,
                     y = word_vec,
                     method = "cosine", norm = "l2") %>% 
        as.data.frame() %>% 
        rename(similarity = V1) %>% 
        mutate(word = dictionary$word) %>% 
        select(word, similarity) %>% 
        filter(word != token[[1, 1]]) %>% 
        arrange(desc(similarity)) %>% 
        top_n(how_many)
      
    } else {
      
      word_vec <- as.matrix(token)
      
      text2vec::sim2(x = dict_vec,
                     y = word_vec,
                     method = "cosine", norm = "l2") %>% 
        as.data.frame() %>% 
        rename(similarity = V1) %>% 
        mutate(word = dictionary$word) %>% 
        select(word, similarity) %>% 
        arrange(desc(similarity)) %>% 
        top_n(how_many)
      
    }
    
  }
  

  
}

### analogy_check()

# This function will aid users in doing analogy checks on word embeddings.
# The function takes 3 words to triangulate an analogy.

# Ex: word_main1 = 'king', word_analog1 = 'man', word_main2 = 'queen'
#    The function will find the vector that connects king and man, add it to
#    queen then find the closest vector.
#    "King is to Man as Queen is to ?"

# word_main1 = main word to anchor analogy
# word_analog1 = analogous word to main word
# word_main2 = word to find analogous word of
# dictionary = word embedding tibble

analogy_check <- function(word_main1, word_analog1, word_main2, dictionary, how_many = 5) {
  
  dict_vec <- 
    dictionary %>% 
    select(-word) %>% 
    as.matrix()
  
  anchor_vec <-   
    dictionary %>%
    filter(word == word_main1) %>% 
    select(-word) %>% 
    as.matrix() 
  
  analog_vec <- 
    dictionary %>%
    filter(word == word_analog1) %>% 
    select(-word) %>% 
    as.matrix() 
  
  connect_vec <- analog_vec - anchor_vec
  
  secondary_vec <- 
    dictionary %>% 
    filter(word == word_main2) %>% 
    select(-word) %>% 
    as.matrix() 
  
  text2vec::sim2(x = dict_vec,
                 y = secondary_vec + connect_vec,
                 method = "cosine", norm = "l2") %>% 
    as.data.frame() %>% 
    rename(similarity = V1) %>% 
    mutate(word = dictionary$word) %>% 
    select(word, similarity) %>% 
    arrange(desc(similarity)) %>% 
    top_n(how_many)
  
}


### combine_doc_vectors

# this function will take a set of document vectors and combine them into one

# docs = character vector of doc id strings
# dioctionary = embedding dictionary in tibble form

combine_docs <- function(docs, dictionary) {
  
  dictionary %>% 
    filter(article_id %in% docs) %>% 
    select(-article_id) %>%
    summarise_all(sum)
  
}

### get_similar_docs()

# This function will take in a document vector and return similar document vectors.

# doc_vec = vector representation of a document
# dictionary = document vector tibble for full vocab
# meta = long metadata table
# how_many = how many similar documents
# docs_used = character vector of docs used if passing a doc combination

get_similar_docs <- function(doc_vec, dictionary, meta, how_many = 5, docs_used = NULL) {
  
  
  if(!is.null(doc_vec$article_id)) {
    
    art_id <- doc_vec$article_id
    
    doc_vec <- as.matrix(doc_vec %>% select(-article_id))
    
    dict_vec <- 
      dictionary %>% 
      select(-article_id) %>% 
      as.matrix()
    
    text2vec::sim2(x = dict_vec,
                   y = doc_vec,
                   method = "cosine", norm = "l2") %>% 
      as.data.frame() %>% 
      rename(similarity = V1) %>% 
      mutate(article_id = dictionary$article_id) %>% 
      select(article_id, similarity) %>%  
      filter(article_id != art_id) %>% 
      arrange(desc(similarity)) %>% 
      top_n(how_many) %>% 
      left_join(meta %>%
                  filter(var == "title") %>%
                  distinct() %>% 
                  select(-var) %>% 
                  rename(title = val)) %>% 
      left_join(meta %>%
                  filter(var == 'summary') %>%
                  distinct() %>% 
                  select(-var) %>% 
                  rename(abstract = val))

    
  } else {
    
    doc_vec <- as.matrix(doc_vec)
    
    dict_vec <- 
      dictionary %>% 
      filter(!article_id %in% docs_used) %>% 
      select(-article_id) %>% 
      as.matrix()
    
    text2vec::sim2(x = dict_vec,
                   y = doc_vec,
                   method = "cosine", norm = "l2") %>% 
      as.data.frame() %>% 
      rename(similarity = V1) %>% 
      mutate(article_id = dictionary$article_id[!dictionary$article_id %in% docs_used]) %>% 
      select(article_id, similarity) %>%  
      arrange(desc(similarity)) %>% 
      top_n(how_many) %>% 
      left_join(meta %>%
                  filter(var == "title") %>%
                  distinct() %>% 
                  select(-var) %>% 
                  rename(title = val)) %>% 
      left_join(meta %>%
                  filter(var == 'summary') %>%
                  distinct() %>% 
                  select(-var) %>% 
                  rename(abstract = val))
    
    
  }
  
}


##### get_similar_words

# Takes a named or unnamed 
