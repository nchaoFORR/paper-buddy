library(tidyverse)
library(haven)
library(jsonlite)
library(httr)



# pull from API

# top 20 posts from march 1st 2019 through april 1st 2019
# address <- "https://api.stackexchange.com/2.2/questions?page=1&pagesize=20&fromdate=1551398400&todate=1554076800&order=desc&sort=votes&site=stackoverflow"


# function to make a get requst from stackoverflow questions
# from is start date
# to is end date
# page is page number

# each page is 100 items long

gen_request_df <- function(from, to, page) {
  
  # from = 1551398400
  # to = 1554076800
  # page = 1
  
  short <- "https://api.stackexchange.com/2.2/questions?"
  
  full <- paste0(
    
    short,
    "page=", page,
    '&pagesize=100&',
    'fromdate=', from,
    '&todate=', to,
    '&order=desc&filter=withbody&sort=votes&site=stackoverflow'
    
  )
  
  
  request <- GET(
    url = full
  )
  
  message(paste0('Status code: ', request$status_code))
  
  # looks good.
  
  # gather data from pull and parse our json
  
  response <- content(request, as = "text", encoding = 'utf-8')
  
  raw_json <- fromJSON(response)
  
  out <- raw_json$items %>% as_tibble()
  
  return(out)
  
}

# test <- gen_request_df(from = 1551398400, to = 1554076800, page = 1)









