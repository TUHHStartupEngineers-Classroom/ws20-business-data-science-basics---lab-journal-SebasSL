library(tidyverse) 
library(rvest)     
library(xopen)     
library(jsonlite)  
library(glue)      
library(stringi)
library(furrr) 
library(httr)

## API REQUEST ---- CAT FACTS API

# resp <- GET("https://cat-fact.herokuapp.com/facts/random?animal_type=cat&amount=10")
# 
# resp <- resp$content %>%
#   rawToChar() %>%
#   fromJSON() %>%
#   as_tibble()

# resp %>% write_rds("00_data/07_data_journal/catFacts.rds")


## WEB SRAPPING ---- RADON-BIKES 

url <- "https://www.radon-bikes.de/en/"

html <- url %>% read_html()


#Category links
bikes_category_links <- html %>%
  html_nodes(css = ".megamenu__item>a") %>%
  html_attr('href') %>%
  discard(.p = ~stringr::str_detect(.x,"wear")) %>%
  enframe(name = "position", value = "subdirectory") %>%
  mutate(url = glue("https://www.radon-bikes.de{subdirectory}bikegrid/")) %>%
  select(-subdirectory)

#Get info bikes function
get_bike_info <- function (url) {
  
  html <- url %>% read_html()
  #get base nodes
  base_nodes <- html %>%
    html_nodes(css = ".m-bikegrid__item>div>a")
  
  #get more info links
  more_info_links <- base_nodes %>%
    html_attr('href') %>%
    enframe(name = "position", value = "subdirectory") %>%
    mutate(url = glue("https://www.radon-bikes.de{subdirectory}")) %>%
    select(-subdirectory)
  
  #get bike model name
  bike_model <- base_nodes %>%
    html_nodes(css = "div>.a-heading") %>%
    html_text() %>%
    stringr::str_extract("(?<=\\n).*(?=\\n)") %>%
    enframe(name = "position", value = "subdirectory") %>%
    rename(model = subdirectory)
  
  bike_model$model <- gsub(" ", "", bike_model$model, fixed = TRUE)
  
  #get bike price
  bike_price <- base_nodes %>%
    html_nodes(css = "div>.currency_eur>.m-bikegrid__price--active") %>%
    html_text() %>%
    enframe(name = "position", value = "subdirectory") %>%
    rename(price = subdirectory)
  
  bikes_category_tbl <- bike_model %>%
    mutate(category = url %>% stringr::str_extract("(?<=en/).*(?=/bikegrid/)") %>% str_replace_all("/","_")) %>%
    left_join(bike_price) %>%
    left_join(more_info_links)
  
} 

url_vector <- bikes_category_links %>%
  pull(url)

data_bikes_tbl <- url_vector %>%
  map(get_bike_info) %>%
  bind_rows()

data_bikes_tbl %>% write_rds("00_data/07_data_journal/data_bikes_tbl.rds")






