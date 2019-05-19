######## Text Analysis Functions

### most_similar()

# this function will return the 10 most similar words.
# word = character vector
# dictionary = word embedding matrix
# how_many = numeric
most_similar <- function(token, dictionary, how_many = 10, named) {
  
  library(tidyverse)
  
  tryCatch({
    
    word_vec <-
      dictionary %>% 
      filter(word == token) %>% 
      select(-word) %>% 
      as.matrix()
    
    dict_vec <- 
      dictionary %>% 
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
