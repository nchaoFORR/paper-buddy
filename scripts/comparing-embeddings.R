library(tidyverse)
library(tidytext)

source('scripts/text-functions.R')


### Compare pre-trained fastext, glove, and combination

unigrams_tidy <-
  read_rds('data/unigrams_small.rds') %>% 
  group_by(word) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count >= 5) %>% 
  select(-count)

### Glove

glove_embedding <- read_rds('data/word-embeddings/custom-glove-embeddings.rds')

glove_doc_embeddings <- 
  unigrams_tidy %>% 
  count(article_id, word) %>% 
  bind_tf_idf(word, article_id, n) %>% 
  select(article_id, word, tf_idf) %>% 
  left_join(glove_embedding) %>%  
  group_by(article_id) %>%
  summarise_at(vars(glove_dim_1:glove_dim_50), ~{weighted.mean(., w = tf_idf)})

### FastText Wikipedia

fastext_wiki_embedding <-
  read_rds('data/word-embeddings/fastext-wiki-pretrained.rds') %>% 
  separate(vec, c(paste0('ft_dim_', 1:300)), " ") %>% 
  mutate_at(vars(ft_dim_1:ft_dim_300), as.numeric)

# Check similar words to math-y words in fastext
# fastext_wiki_mat <- as.matrix(fastext_wiki_embedding)
# row.names(fastext_wiki_embedding) <- fastext_wiki_embedding[, 1] %>% pull()
# fastext_wiki_mat <- fastext_wiki_mat[, 2:ncol(fastext_wiki_mat)]
# 
# most_similar("boston", fastext_wiki_mat)

fastext_doc_embedding <- 
  unigrams_tidy %>% 
  count(article_id, word) %>% 
  bind_tf_idf(word, article_id, n) %>% 
  select(article_id, word, tf_idf) %>% 
  left_join(fastext_wiki_embedding) %>% 
  drop_na() %>% 
  group_by(article_id) %>%
  summarise_at(vars(ft_dim_1:ft_dim_300), ~{weighted.mean(., w = tf_idf)})

write_rds(fastext_doc_embedding, 'data/fastextwiki_doc_embeddings.rds')

