library(tidyverse)
library(jsonlite)
library(xml2)
library(httr)

### Grab 100 pages containing the word algorithm

### Batch process 100 pages per pull



### Setup urls to loop through

search_terms <-
  unigrams_tidy %>% 
  sample_n(20) %>% 
  pull()

search_terms <- c('gene', 'expression', "demand", 'production', 'wave', 'pull')

for(term in search_terms) {
  
  by_100 <- seq(from = 0, to = 30000, by = 2000)
  
  by_100_url <- 
    map_chr(by_100, ~{
      paste0('http://export.arxiv.org/api/query?search_query=all:',
             term ,
             '&start=',
             .,
             '&max_results=',
             . + 2000)
    })
  
  ### Loop through urls and grab the response from the get request
  
  data_pull <-
    map(by_100_url, ~{
      
      request <- GET(
        url = .
      )
      
      message(paste0('Status code: ', request$status_code))
      
      # only return data if it was succesful
      if(request$status_code == 200) {
        
        response <- content(request, as = "text", encoding = 'utf-8')
        
      } else {
        message('an error occured')
        return()
      }
      
      # owners recommend 3 second sleep  
      Sys.sleep(3)
      
      response
      
      
    })
  
  ### Loop through the results and parse out the data to the long format
  
  full_long_df <- map_dfr(data_pull[1:5], function(response) {
    
    # response = data_pull[[3]]
    
    parsed_xml <- read_xml(response)
    
    # some responses are empty, so handle errors
    
    tryCatch({
      
      paper_nodes <- parsed_xml %>%
        xml_children %>% 
        .[8:length(.)]
      
    },
    error = function(e) {
      return()
    })
    
    map_dfr(paper_nodes, function(node) {
      
      # node = paper_nodes[1]
      
      map_dfr(node %>% xml_children, function(child) {
        
        # child = node %>% xml_children %>% .[1]
        
        tibble(
          var = child %>% xml_name(),
          val = child %>% xml_text()
        )
        
      }) %>% 
        mutate(article_id = val[var == "id"]) %>% 
        select(article_id, var, val)
      
    })
    
  })
  
  
  ### Dedupe
  
  full_long_df %>% 
    group_by(article_id) %>% 
    summarise(count = n()) %>% 
    ggplot(aes(count)) +
    geom_histogram()
  
  final_df <- full_long_df %>% distinct()
  
  final_df %>% 
    filter(var == "title") %>% 
    distinct() %>% 
    nrow()
  
  write_rds(final_df, paste0('data/', term, '.rds'))
  
  
}

