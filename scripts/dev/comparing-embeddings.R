library(tidyverse)
library(tidytext)
library(text2vec)
library(plotly)
library(ggwordcloud)

source('scripts/functions/text-functions.R')


### Compare pre-trained fastext, glove, and combination

# cnnb_docs <- read_rds('data/word-embeddings/conceptnet_numberbatch_tibble.rds')
pca_docs <- read_rds('data/pca-doc-embedding.rds')
svd_docs <- read_rds('data/svd-doc-embedding.rds')

# read in document metadata

files <- dir('data/arxiv')

doc_meta <- map_dfr(paste0('data/arxiv/', files), read_rds) %>% distinct()

# write_rds(doc_meta, 'Paperman-Prototype/data/doc_meta_medium.rds')

# read in word embedding dictionaries
glove_dict <- read_rds('data/word-embeddings/custom-glove-embeddings.rds')
pca_dict <- read_rds('data/word-embeddings/concat_pca_tibble.rds')
svd_dict <- read_rds('data/word-embeddings/concat_svd_tibble.rds')

# Grab closest word to each doc for viz

### This function takes a doc tibble and grabs the word with the highest tf-idf
### for each doc to create a word embedding tibble

# doc_tibble = a doc embedding tibble
# dictionary = a word embedding dictionary for the same embedded space
doc_tf_idf <- function(doc_tibble, dictionary, how_many = 1) {
  
  most_similar(doc_tibble, dictionary)
  
}

### Quick tsne comp -- color by journal

tsne_glove <- Rtsne::Rtsne(glove_docs %>% select(-article_id) %>% as.matrix,
                             initial_dims = 300, verbose = TRUE, check_duplicates = FALSE)
tsne_pca <- Rtsne::Rtsne(pca_docs %>% select(-article_id) %>% as.matrix,
                              initial_dims = 26, verbose = TRUE, check_duplicates = FALSE)
tsne_svd <- Rtsne::Rtsne(svd_docs %>% select(-article_id) %>% as.matrix,
                              initial_dims = 26, verbose = TRUE, check_duplicates = FALSE)

### Plot docs with highest tf-idf words as label
unigrams_tidy <- read_rds('data/unigrams_medium.rds')

unigrams_tf_idf <- 
  unigrams_tidy %>% 
  count(article_id, word) %>% 
  bind_tf_idf(word, article_id, n) %>% 
  group_by(article_id) %>% 
  filter(tf_idf == max(tf_idf)) %>% 
  ungroup()


glove_plot <- tibble(
  article_id = glove_docs$article_id
) %>% 
  bind_cols(tsne_glove$Y %>% as.data.frame) %>% 
  left_join(unigrams_tf_idf %>% select(article_id, word)) %>% 
  sample_n(10000) %>% 
  ggplot(aes(x = V1, y = V2, label = word)) +
  geom_text_wordcloud()

svd_plot <- tibble(
  article_id = svd_docs$article_id
) %>% 
  bind_cols(tsne_svd$Y %>% as.data.frame) %>% 
  left_join(doc_meta %>%
              filter(var == "title") %>%
              distinct() %>% 
              rename(title = val) %>% 
              select(-var)) %>% 
  inner_join(doc_meta %>%
               filter(var == "journal_ref") %>%
               distinct() %>% 
               rename(journal = val) %>% 
               select(-var)) %>% 
  ggplot(aes(V1, V2, label = word, col = journal)) +
  geom_point(size = 0.7, alpha = 0.7)

pca_plot <- tibble(
  article_id = pca_docs$article_id
) %>% 
  bind_cols(tsne_pca$Y %>% as.data.frame) %>% 
  left_join(doc_meta %>%
              filter(var == "title") %>%
              distinct() %>% 
              rename(title = val) %>% 
              select(-var)) %>% 
  inner_join(doc_meta %>%
               filter(var == "journal_ref") %>%
               distinct() %>% 
               rename(journal = val) %>% 
               select(-var)) %>% 
  ggplot(aes(V1, V2, label = journal, col = journal)) +
  geom_point(size = 0.7, alpha = 0.7)

ggplotly(glove_plot, tooltip = "label")
ggplotly(svd_plot, tooltip = "label")
ggplotly(pca_plot, tooltip = "label")

#######################

### Compare on document retrieval

# (Neural Bayesian Word Embeddings)
test_id <- 'http://arxiv.org/abs/1603.06571v3'
# (Low-dimensional Embeddings for Anchor Words)
test_id <- 'http://arxiv.org/abs/1711.06826v1'
# (A practical topic model with provable)
test_id <- 'http://arxiv.org/abs/1212.4777v1'

test_set <- c('http://arxiv.org/abs/1603.06571v3',
              'http://arxiv.org/abs/1711.06826v1',
              'http://arxiv.org/abs/1212.4777v1',
              # Astronomy stuff
              'http://arxiv.org/abs/0903.2805v2',
              # Astronomy stuff
              'http://arxiv.org/abs/1611.07526v1',
              'http://arxiv.org/abs/astro-ph/0607281v1')

# realistic search history for me
test_set <- c('http://arxiv.org/abs/1711.06826v1', # mimno anchor words
              'http://arxiv.org/abs/1602.08644v3', # market theory
              'http://arxiv.org/abs/1708.08275v1', # capital growth theory
              'http://arxiv.org/abs/1707.04860v1') # word embedding comparison

###

test_doc <- combine_docs(test_set, glove_docs)
# test_doc <- glove_docs[glove_docs$article_id == test_id, ]

doc_meta %>% 
  filter(article_id == test_doc$article_id)

get_similar_docs(test_doc, glove_docs, doc_meta,
                 how_many = 10, docs_used = test_set) %>% View()


###

test_doc <- combine_docs(test_set, pca_docs)
# test_doc <- pca_docs[pca_docs$article_id == test_id, ]



get_similar_docs(test_doc, pca_docs, doc_meta,
                 how_many = 10, docs_used = test_set) %>% View()

###

test_doc <- combine_docs(test_set, svd_docs)
# test_doc <- svd_docs[svd_docs$article_id == test_id, ]



get_similar_docs(test_doc, svd_docs, doc_meta,
                 how_many = 10, docs_used = test_set) %>% View()


