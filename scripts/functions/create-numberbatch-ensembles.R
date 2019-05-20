library(tidyverse)
library(tidytext)

cn_nb <- read_rds('data/word-embeddings/conceptnet_numberbatch.rds')

unigrams_tidy <- read_rds('data/unigrams_small.rds')

cnnb_embeddings <- 
  unigrams_tidy %>%
  count(article_id, word) %>% 
  bind_tf_idf(word, article_id, n) %>% 
  inner_join(cn_nb) %>% 
  group_by(article_id) %>% 
  summarise_at(vars(cnnb_dim_1:cnnb_dim_300), ~{weighted.mean(., w = tf_idf)})

write_rds(cnnb_embeddings, 'data/cnnb_doc_embeddings.rds')


glove <- read_rds('data/word-embeddings/custom-glove-embeddings.rds')

glove
