######## Text Analysis Functions

### most_similar()

# this function will return the 10 most similar words.
# word = character vector, dictionary = word embedding matrix,  how_many = numeric
most_similar <- function(word, dictionary, how_many = 10) {
  
  library(dplyr)
  
  word_vectors <- dictionary
  
  tryCatch({
    sim_df <- 
      text2vec::sim2(x = word_vectors[word_vectors %>% attr('dimnames') %>% .[[1]] != word, , drop = FALSE],
           y = word_vectors[word, , drop = FALSE],
           method = "cosine", norm = "l2") %>% 
      as.data.frame() %>% 
      tibble::rownames_to_column('word') 
    
    names(sim_df)[2] <- 'similarity'
    
    sim_df %>% 
      arrange(desc(similarity)) %>% 
      top_n(how_many)
    
  },
  error = function(e) {
    
    message('Word not found in dictionary!')
    return()
    
  })
  
}
