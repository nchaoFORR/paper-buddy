library(tidyverse)
library(tidytext)
library(text2vec)

### Create Glove embeddings

unigrams <- read_rds('data/unigrams_small.rds')

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

tcm <- create_tcm(it, vectorizer, skip_grams_window = 5)

## Fit Glove

glove_fit <- GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab_t2v,
                               x_max = 8, lambda = 1e-5)

wv_main <- glove_fit$fit_transform(tcm, n_iter = 20, convergence_tol = 0.01)

# grab context vecs and combine

wv_context <- glove_fit$components

word_vectors <- wv_main + t(wv_context)


### Sanity check

# most similar words to gaussian

sim_to_gaussian <-
  sim2(x = word_vectors,
       y = word_vectors['gaussian', , drop = FALSE],
       method = "cosine", norm = "l2") %>% 
  as.data.frame() %>% 
  rownames_to_column('word') %>%
  rename(similarity = gaussian) %>% 
  arrange(desc(similarity))

sim_to_gaussian %>% head()

# analogy check -- gaussian to mixture vs markox to chain?

gaussian_to_mixture <-
  t(word_vectors['gaussian', ]) - t(word_vectors['mixture', ])

markov_to_chain <- 
  t(word_vectors['markov', ]) - t(word_vectors['chain', ])

sim2(x = gaussian_to_mixture, y = markov_to_chain,
     method = "cosine", norm = 'l2')

## not terrible

## T-sne explore

tsne_out <- Rtsne::Rtsne(word_vectors, initial_dims = 50, perplexity = 35,
                         verbose = TRUE)

