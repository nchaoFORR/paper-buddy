library(tidyverse)
library(tidytext)

source('scripts/functions/text-functions.R')

### Read in embeddings

cnnb <- read_rds('data/word-embeddings/conceptnet_numberbatch_tibble.rds')
glove <- read_rds('data/word-embeddings/custom-glove-embeddings.rds')

### Concatonate

concat <-
  glove %>% 
  left_join(cnnb) %>% 
  mutate_at(vars(cnnb_dim_1:cnnb_dim_300), ~{ifelse(is.na(.), 0, .)}) %>% 
  mutate_at(vars(-word), scale)


### 7000 words missing in numberbatch is far too many to give up -- replace with zeros

### SVD

svd_out <- svd(concat %>% select(-word) %>% as.matrix)

tibble(
  sv = 1:450,
  scale = svd_out$d
) %>% 
  ggplot(aes(sv, scale)) +
  geom_line() +
  geom_text(aes(label = sv))

# 150 dims looks appropriate

svd_embeddings <- svd_out$u[, 1:150] %*% diag(svd_out$d[1:150]) %*% t(svd_out$v[1:150, 1:150])

svd_tibble <- 
  svd_embeddings %>% 
  as.data.frame() %>% 
  mutate(word = concat$word) %>% 
  select(word, everything()) %>% 
  as_tibble()

### PCA

pca_out <- prcomp(concat %>% select(-word) %>% as.matrix)

var_explained <- pca_out$sdev**2 / sum(pca_out$sdev**2)

tibble(prin_comp = 1:450,
       var_expl = var_explained) %>% 
  ggplot(aes(prin_comp, var_explained)) +
  geom_line()


pca_tibble <- 
  pca_out$x[, 1:150] %>% 
  as.data.frame() %>% 
  mutate(word = concat$word) %>% 
  select(word, everything())


### Compare

test_word <- 'elasticity'

most_similar(test_word, cnnb, how_many = 15)
most_similar(test_word, glove, how_many = 15)
most_similar(test_word, concat, how_many = 15)
most_similar(test_word, pca_tibble, how_many = 15)
most_similar(test_word, svd_tibble, how_many = 15)

word1 <- 'protein'
word2 <- 'cell'
word3 <- 'word'

analogy_check(word1, word2, word3, cnnb, how_many = 8)
analogy_check(word1, word2, word3, glove, how_many = 8)
analogy_check(word1, word2, word3, concat, how_many = 8)
analogy_check(word1, word2, word3, pca_tibble, how_many = 8)
analogy_check(word1, word2, word3, svd_tibble, how_many = 8)

### It seems like SVD incorporates a bit more of the glove
### embedding than the pca does (might just be my head)

### Either way, blending the embeddings is definitely the way to go

write_rds(svd_tibble, 'data/word-embeddings/concat_svd_tibble.rds')
write_rds(pca_tibble, 'data/word-embeddings/concat_pca_tibble.rds')
