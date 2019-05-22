library(tidyverse)
library(tidytext)
library(text2vec)
library(plotly)

source('scripts/functions/text-functions.R')

### Create Glove embeddings

unigrams <- read_rds('data/unigrams_medium.rds')

### Initial clean (words are already lemma'd and stop words removed)

unigrams_common <- 
  unigrams %>%
  group_by(word) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count >= 5) %>% 
  select(-count)

vocab <- unigrams_common$word %>% unique()

docs_df <- 
  unigrams_common %>% 
  group_by(article_id) %>% 
  summarise(doc = str_c(word, collapse = " "))

### 26,000 words

### Fit Glove

# Create iterator and vocab for glove format

tokens <- tokenizers::tokenize_words(docs_df$doc)

it <- itoken(tokens, progressbar = TRUE)

vocab_t2v <- create_vocabulary(it)

# Create term co-occurence matrix

vectorizer <- vocab_vectorizer(vocab_t2v)

tcm <- create_tcm(it, vectorizer, skip_grams_window = 7)

## Fit Glove

glove_fit <- GlobalVectors$new(word_vectors_size = 150, vocabulary = vocab_t2v,
                               x_max = 8, lambda = 1e-5)

wv_main <- glove_fit$fit_transform(tcm, n_iter = 20, convergence_tol = 0.01)

# grab context vecs and combine

wv_context <- glove_fit$components

word_vectors <- wv_main + t(wv_context)


### Sanity check

## this function will grab the 10 most similar words
most_similar <- function(word, how_many = 10) {
  
  tryCatch({
    sim_df <- 
      sim2(x = word_vectors[word_vectors %>% attr('dimnames') %>% .[[1]] != word, , drop = FALSE],
           y = word_vectors[word, , drop = FALSE],
           method = "cosine", norm = "l2") %>% 
      as.data.frame() %>% 
      rownames_to_column('word') 
    
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


most_similar('gauss')
most_similar('gaussian')
most_similar('bayesian')
most_similar('bayes')
most_similar('poisson')

most_similar('person')



## T-sne explore

tsne_out <- Rtsne::Rtsne(word_vectors, initial_dims = 150, perplexity = 35,
                         verbose = TRUE)


tsne_df <- tibble(
  word = word_vectors %>% attr('dimnames') %>% .[[1]]
) %>% 
  bind_cols(tsne_out$Y %>% as.data.frame())

tsne_df %>% 
  ggplot(aes(V1, V2, label = word)) +
  geom_text(alpha = 0.6, size = 0.3)

p <- 
  tsne_df %>% 
  ggplot(aes(V1, V2, label = word)) +
  geom_point(alpha = 0.6, size = 1)

ggplotly(p, tooltip = 'label')

### I'm reasonably satisfied with these embeddings.

final_embedding <- 
  word_vectors %>% 
  as.data.frame() %>% 
  rownames_to_column('word')

names(final_embedding)[2:151] <- paste0('glove_dim_', 1:150)

write_rds(final_embedding, 'data/word-embeddings/custom-glove-embeddings.rds')
write_csv(final_embedding, 'data/word-embeddings/custom-glove-embeddings.csv')
