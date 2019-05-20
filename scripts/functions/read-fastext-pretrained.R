library(tidyverse)


raw_vectors <- read_lines('../../word-embeddings/fastext-wikipedia/wiki-news-300d-1M.vec',
           skip = 1, progress = TRUE, n_max = 1000000)

### Create 2-d matrix where dim1 is word and dim2 is space-separated dimension
### string. This lets us use a 2-d vector for words until we only have
### the vectors we need.

# check longest length of word
i <- 1
final_df <- map_dfr(raw_vectors, function(vec){
  
  # vec = raw_vectors[[1]]
  message(i)
  i <<- i + 1  
  tibble(word = str_extract(vec, "[^ ]{1,50}(?= )"),
         vec = str_extract(vec, "(?<=[^ ] ).*"))
  
})

write_rds(final_df, "data/word-embeddings/fastext-wiki-pretrained.rds")
write_csv(final_df, "data/word-embeddings/fastext-wiki-pretrained.csv")
