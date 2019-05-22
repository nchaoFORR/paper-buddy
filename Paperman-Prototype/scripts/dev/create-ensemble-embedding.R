library(tidyverse)
library(tidytext)
library(text2vec)

### Create ensemble-embedding by binding the embeddings-colwise and 
### using SVD

glove_embeddings <- read_rds('data/word-embeddings/custom-glove-embeddings.rds')

fastext_embeddings_raw <- read_rds('data/word-embeddings/fastext-wiki-pretrained.rds')

# append
ensemble_temp <-
  glove_embeddings %>% 
  left_join(fastext_embeddings_raw) %>% 
  mutate(vec = ifelse(is.na(vec), str_c(rep(0, 300), collapse = " "), vec)) %>% 
  separate(vec, c(paste0('ft_dim_', 1:300)), " ") %>% 
  mutate_at(vars(ft_dim_1:ft_dim_300), as.numeric) %>% 
  mutate_at(vars(glove_dim_1:ft_dim_300), scale)

fastext_embeddings <- ensemble_temp %>%
  select(word, contains('ft'))

# svd

sing_val <- svd(ensemble_temp %>% select(-word) %>% as.matrix)

tibble(
  sv = 1:350,
  scale = sing_val$d
) %>% 
  ggplot(aes(sv, scale)) +
  geom_line() +
  geom_text(aes(label = sv))

# reconstruct data with top 26 singular values

ensemble_temp2 <- sing_val$u[, 1:26] %*% diag(sing_val$d[1:26]) %*% t(sing_val$v[1:26, 1:26])


ensemble_embedding <- ensemble_temp2 %>% 
  as.data.frame() %>% 
  mutate(word = ensemble_temp$word) %>% 
  select(word, everything()) %>% 
  as_tibble()

### Top similarity check

most_similar('gaussian', ensemble_embedding, how_many = 25)
most_similar('gaussian', glove_embeddings, how_many = 25)
most_similar('gaussian', fastext_embeddings, how_many = 25)


most_similar('bayesian', ensemble_embedding, how_many = 25)
most_similar('bayesian', glove_embeddings, how_many = 25)
most_similar('bayesian', fastext_embeddings, how_many = 25)


most_similar('genome', ensemble_embedding, how_many = 25)
most_similar('genome', glove_embeddings, how_many = 25)
most_similar('genome', fastext_embeddings, how_many = 25)

most_similar('customer', ensemble_embedding, how_many = 25)
most_similar('customer', glove_embeddings, how_many = 25)
most_similar('customer', fastext_embeddings, how_many = 25)

### Analogy check

analogy_check('chemistry', 'cell', 'astronomy', ensemble_embedding)
analogy_check('chemistry', 'cell', 'astronomy', glove_embeddings)
analogy_check('chemistry', 'cell', 'astronomy', fastext_embeddings)

analogy_check('astronomy', 'satellite', 'economics', ensemble_embedding)
analogy_check('astronomy', 'satellite', 'economics', glove_embeddings)
analogy_check('astronomy', 'satellite', 'economics', fastext_embeddings)

analogy_check('astronomy', 'satellite', 'biology', ensemble_embedding)
analogy_check('astronomy', 'satellite', 'biology', glove_embeddings)
analogy_check('astronomy', 'satellite', 'biology', fastext_embeddings)

analogy_check('astronomy', 'satellite', 'genome', ensemble_embedding)
analogy_check('astronomy', 'satellite', 'genome', glove_embeddings)
analogy_check('astronomy', 'satellite', 'genome', fastext_embeddings)


### From this eye-test, ensemble algorithm looks very good, and only contains
### 27 dimension!

write_rds(ensemble_embedding, 'data/ensemble-embedding.rds')
write_csv(ensemble_embedding, 'data/ensemble-embedding.csv')
