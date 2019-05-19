library(tidyverse)
library(tidytext)
library(text2vec)
library(plotly)

source('scripts/text-functions.R')


### Compare pre-trained fastext, glove, and combination

fastext_docs <- read_rds('data/fastextwiki_doc_embeddings.rds')
glove_docs <- read_rds('data/glove_doc_embeddings.rds')

# read in document metadata

doc_meta <- read_rds('data/algorithm_small.rds')

### Quick tsne comp -- color by journal

tsne_fastext <- Rtsne::Rtsne(fastext_docs %>% select(-article_id) %>% as.matrix,
                             initial_dims = 300, verbose = TRUE, check_duplicates = FALSE)
tsne_glove <- Rtsne::Rtsne(glove_docs %>% select(-article_id) %>% as.matrix,
                             initial_dims = 300, verbose = TRUE, check_duplicates = FALSE)


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

ggplotly(fastext_plot, tooltip = "label")
ggplotly(glove_plot, tooltip = "label")

### Custom embedding seems to do a better job of differetiating journals
###   -- Will combining them improve ability to blend computation method distinction
###      as well we industry.

