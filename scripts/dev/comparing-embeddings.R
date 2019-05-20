library(tidyverse)
library(tidytext)
library(text2vec)
library(plotly)

source('scripts/text-functions.R')


### Compare pre-trained fastext, glove, and combination

fastext_docs <- read_rds('data/fastextwiki_doc_embeddings.rds')
glove_docs <- read_rds('data/glove_doc_embeddings.rds')
ensemble_docs <- read_rds('data/ensemble-doc-embedding.rds')

# read in document metadata

doc_meta <- read_rds('data/algorithm_small.rds')

### Quick tsne comp -- color by journal

tsne_fastext <- Rtsne::Rtsne(fastext_docs %>% select(-article_id) %>% as.matrix,
                             initial_dims = 300, verbose = TRUE, check_duplicates = FALSE)
tsne_glove <- Rtsne::Rtsne(glove_docs %>% select(-article_id) %>% as.matrix,
                             initial_dims = 300, verbose = TRUE, check_duplicates = FALSE)
tsne_ensemble <- Rtsne::Rtsne(ensemble_docs %>% select(-article_id) %>% as.matrix,
                              initial_dims = 26, verbose = TRUE, check_duplicates = FALSE)


fastext_plot <- tibble(
  article_id = fastext_docs$article_id
) %>% 
  bind_cols(tsne_fastext$Y %>% as.data.frame) %>% 
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
  ggplot(aes(V1, V2, label = journal)) +
  geom_point(size = 0.7, alpha = 0.7)

glove_plot <- tibble(
  article_id = glove_docs$article_id
) %>% 
  bind_cols(tsne_glove$Y %>% as.data.frame) %>% 
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
  ggplot(aes(V1, V2, label = journal)) +
  geom_point(size = 0.7, alpha = 0.7)

ensemble_plot <- tibble(
  article_id = ensemble_docs$article_id
) %>% 
  bind_cols(tsne_ensemble$Y %>% as.data.frame) %>% 
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
  ggplot(aes(V1, V2, label = journal)) +
  geom_point(size = 0.7, alpha = 0.7)

ggplotly(fastext_plot, tooltip = "label")
ggplotly(glove_plot, tooltip = "label")
ggplotly(ensemble_plot, tooltip = "label")

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

### Ensemble

test_doc <- combine_docs(test_set, ensemble_docs)
# test_doc <- ensemble_docs[ensemble_docs$article_id == test_id, ]

test_doc_info <- doc_meta %>% 
  filter(article_id == test_doc$article_id)

get_similar_docs(test_doc, ensemble_docs, doc_meta,
                 how_many = 10, docs_used = test_set) %>% View()


### Glove

test_doc <- combine_docs(test_set, glove_docs)
# test_doc <- glove_docs[glove_docs$article_id == test_id, ]

test_doc_info <- doc_meta %>% 
  filter(article_id == test_doc$article_id)

get_similar_docs(test_doc, glove_docs, doc_meta,
                 how_many = 10, docs_used = test_set) %>% View()

### FastText Pre-trained

test_doc <- combine_docs(test_set, fastext_docs)
# test_doc <- fastext_docs[fastext_docs$article_id == test_id, ]

test_doc_info <- doc_meta %>% 
  filter(article_id == test_doc$article_id)

get_similar_docs(test_doc, fastext_docs, doc_meta,
                 how_many = 10, docs_used = test_set) %>% View()


