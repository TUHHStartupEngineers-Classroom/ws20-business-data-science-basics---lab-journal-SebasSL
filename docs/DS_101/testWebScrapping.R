library(rvest)
library(tidyverse)
library(httr)
library(purrr)

url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

# sp_500 <- url %>%
#   read_html() %>%
#   html_nodes(css = "#constituents") %>%
#   html_table() %>% 
#   .[[1]] %>% 
#   as_tibble()
# 
# glimpse(sp_500)


#IMDB
# 
# url  <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"
# 
# resp <- GET(url = "https://www.imdb.com/chart/top/?ref_=nv_mv_250",  
#             add_headers('Accept-Language' = "en-US, en;q=0.5")) 
# html <- content(resp)
# 
# #Rank
# rank <-  html %>% 
#   html_nodes(css = ".titleColumn") %>% 
#   html_text() %>% 
#   stringr::str_extract("(?<= )[0-9]*(?=\\.\\n)")%>% 
#   as.numeric()
# 
# glimpse(rank)
# 
# #Titles
# titles <-  html %>% 
#   html_nodes(css = ".titleColumn>a") %>% 
#   html_text()
# 
# glimpse(titles)
# 
# #Year
# years <- html %>%
#   html_nodes(css = ".titleColumn>.secondaryInfo") %>%
#   html_text() %>%
#   stringr::str_extract("\\d+")
# 
# glimpse(years)
# 
# #People
# people <- html %>%
#   html_nodes(css = ".titleColumn>a") %>%
#   html_attr('title')
# 
# glimpse(people)
# 
# #Rating
# rating <- html %>%
#   html_nodes(css = ".ratingColumn>strong") %>%
#   html_text()%>% 
#   as.numeric()
# 
# 
# glimpse(rating)
# 
# # Num. ratings
# num_ratings <- html %>%
#   html_nodes(css = '.ratingColumn>strong') %>%
#   html_attr('title') %>%
#   stringr::str_extract("(?<=on ).*(?=\ user)") %>% 
#   stringr::str_replace_all(pattern = ",", replacement = "") %>% 
#   as.numeric()
# 
# 
# glimpse(num_ratings)

# bike_data_lst <- fromJSON("./00_data/03_bike_json/bike_data.json")
# # Open the data by clicking on it in the environment or by running View()
# View(bike_data_lst)



