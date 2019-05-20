library(tidyverse)
library(tidytext)

files <- dir('data/arxiv')

raw_df <- map_dfr(paste0('data/arxiv/', files), read_rds)



### Cleaning abstract data to create document vectors.

### Grab and widen data

# must select variables before spreading due to some papers have multiple authors

variables_to_grab <- c('article_id', 'summary')

raw_tibble <- raw_df %>%
  # remove duplicates
  distinct() %>% 
  filter(var %in% variables_to_grab) %>% 
  spread('var', 'val')

### cleaning and eda

# unigrams

unigram_tidy <- raw_tibble %>%
  mutate(summary = str_replace_all(summary, "[0-9]", "")) %>% 
  unnest_tokens(word, summary, to_lower = TRUE) %>% 
  # lemma-tize
  left_join(lexicon::hash_lemmas, by = c('word' = 'token')) %>%
  mutate(word = ifelse(!is.na(lemma), lemma, word)) %>% 
  select(-lemma) %>% 
  # remove stopwords
  filter(!word %in% stop_words$word)

unigram_tidy %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  top_n(20) %>% 
  ggplot(aes(reorder(word, n), n)) +
  geom_col() +
  coord_flip()

# bigrams

bigram_tidy <- raw_tibble %>%
  mutate(summary = str_replace_all(summary, "[0-9]", "")) %>% 
  unnest_tokens(word, summary, token = "ngrams", n = 2,  to_lower = TRUE) %>% 
  # lemma-tize
  separate(word, c('word1', 'word2'), sep = " ") %>% 
  left_join(lexicon::hash_lemmas, by = c('word1' = 'token')) %>%
  mutate(word1 = ifelse(!is.na(lemma), lemma, word1)) %>% 
  select(-lemma) %>% 
  left_join(lexicon::hash_lemmas, by = c('word2' = 'token')) %>%
  mutate(word2 = ifelse(!is.na(lemma), lemma, word2)) %>% 
  select(-lemma) %>% 
  # remove stopwords
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  # unite
  unite(word, word1, word2)


bigram_tidy %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  top_n(20) %>% 
  ggplot(aes(reorder(word, n), n)) +
  geom_col() +
  coord_flip()

# Trigrams

trigram_tidy <- raw_tibble %>%
  mutate(summary = str_replace_all(summary, "[0-9]", "")) %>% 
  unnest_tokens(word, summary, token = "ngrams", n = 3,  to_lower = TRUE) %>% 
  # lemma-tize
  separate(word, c('word1', 'word2', 'word3'), sep = " ") %>% 
  left_join(lexicon::hash_lemmas, by = c('word1' = 'token')) %>%
  mutate(word1 = ifelse(!is.na(lemma), lemma, word1)) %>% 
  select(-lemma) %>% 
  left_join(lexicon::hash_lemmas, by = c('word2' = 'token')) %>%
  mutate(word2 = ifelse(!is.na(lemma), lemma, word2)) %>% 
  select(-lemma) %>% 
  left_join(lexicon::hash_lemmas, by = c('word3' = 'token')) %>%
  mutate(word3 = ifelse(!is.na(lemma), lemma, word3)) %>% 
  select(-lemma) %>% 
  # remove stopwords
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word1 %in% c('algorithm', 'propose', 'paper',
                       'model', 'method', 'compute', 'solve')) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word2 %in% c('algorithm', 'propose', 'paper',
                       'model', 'method', 'compute', 'solve')) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  filter(!word3 %in% c('algorithm', 'propose', 'paper',
                       'model', 'method', 'compute', 'solve')) %>% 
  # unite
  unite(word, word1, word2, word3)


trigram_tidy %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  top_n(20) %>% 
  ggplot(aes(reorder(word, n), n)) +
  geom_col() +
  coord_flip()


# Fourgrams

fourgram_tidy <- raw_tibble %>%
  mutate(summary = str_replace_all(summary, "[0-9]", "")) %>% 
  unnest_tokens(word, summary, token = "ngrams", n = 4,  to_lower = TRUE) %>% 
  # lemma-tize
  separate(word, c('word1', 'word2', 'word3', 'word4'), sep = " ") %>% 
  left_join(lexicon::hash_lemmas, by = c('word1' = 'token')) %>%
  mutate(word1 = ifelse(!is.na(lemma), lemma, word1)) %>% 
  select(-lemma) %>% 
  left_join(lexicon::hash_lemmas, by = c('word2' = 'token')) %>%
  mutate(word2 = ifelse(!is.na(lemma), lemma, word2)) %>% 
  select(-lemma) %>% 
  left_join(lexicon::hash_lemmas, by = c('word3' = 'token')) %>%
  mutate(word3 = ifelse(!is.na(lemma), lemma, word3)) %>% 
  select(-lemma) %>% 
  left_join(lexicon::hash_lemmas, by = c('word4' = 'token')) %>%
  mutate(word4 = ifelse(!is.na(lemma), lemma, word4)) %>% 
  select(-lemma) %>% 
  # remove stopwords
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word1 %in% c('algorithm', 'propose', 'paper',
                       'model', 'method', 'compute', 'solve')) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word2 %in% c('algorithm', 'propose', 'paper',
                       'model', 'method', 'compute', 'solve')) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  filter(!word3 %in% c('algorithm', 'propose', 'paper',
                       'model', 'method', 'compute', 'solve')) %>% 
  filter(!word4 %in% stop_words$word) %>% 
  filter(!word4 %in% c('algorithm', 'propose', 'paper',
                       'model', 'method', 'compute', 'solve')) %>% 
  # unite
  unite(word, word1, word2, word3, word4)


fourgram_tidy %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  top_n(20) %>% 
  ggplot(aes(reorder(word, n), n)) +
  geom_col() +
  coord_flip()


### 4 grams doesn't look too good -- save uni, bi, and tri, and start with
### unigrams for modeling


write_rds(unigram_tidy, 'data/unigrams_medium.rds')
write_rds(bigram_tidy, 'data/bigrams_small.rds')
write_rds(trigram_tidy, 'data/trigrams_small.rds')

