library(tidyverse) 
library(rvest)     
library(xopen)     
library(jsonlite)  
library(glue)      
library(stringi)
library(furrr)  

plan("multiprocess")

url <- "https://www.canyon.com/en-de/road-bikes/"

html <- url %>% read_html()

#Get the bike product family IDs

product_family <- html %>%
  html_node(css = "#js-navigationList-ROAD>dl") %>%
  html_nodes(css = "dd>ul>li>a>span") %>%
  html_text()

# glimpse(product_family)

#Get bike product category urls
product_list_link <- html %>%
  html_nodes(css = ".js-thirdLevelCategory.navigationListSecondary__link") %>%
  html_attr('href') %>%
  enframe(name = "position", value = "subdirectory") %>%
  mutate(
    url = glue("https://www.canyon.com{subdirectory}")
  ) %>%
  distinct(url)

# print(product_list_link)


#get url for one bike
url_cat <- product_list_link$url[2]
# 
# # xopen(url_cat);
# 
# html_bike_category <- read_html(url_cat)
# 
# url_bikes <- html_bike_category %>%
#   html_nodes(css = ".productTile__link") %>%
#   html_attr('href') %>%
#   str_remove(pattern = "\\?.*") %>%
#   enframe(name = "position", value = "subdirectory")
# 
# # print(url_bikes)
# 
# 
# #Description from the example
# bike_desc_tbl <- html_bike_category %>%
#   html_nodes('.productTile__productSummaryLeft > meta[itemprop="description"]') %>%
#   html_attr("content") %>%
#   enframe(name = "position", value = "description")
# 
# # print(bike_desc_tbl)
# 
# #Get JSON data
# 
# #data-gtm-impression
# 
# JSON_data_bikes <- html_bike_category %>%
#   html_nodes(css = ".productTile") %>%
#   html_attr('data-gtm-impression') %>%
#   map(fromJSON) %>%
#   map(purrr::pluck, 2, "impressions") %>%
#   map(na_if, 'not defined') %>%
#   map(na_if, "") %>%
#   map(~mutate(., across(c("dimension56","price"), as.numeric))) %>%
#   bind_rows() %>%
#   as_tibble() %>%
#   rowid_to_column(var = "position") %>%
#   left_join(bike_desc_tbl) %>%
#   left_join(url_bikes)


get_data_bikes <- function (url) {
  html_bike_category <- read_html(url)
  
  url_bikes <- html_bike_category %>%
    html_nodes(css = ".productTile__link") %>%
    html_attr('href') %>%
    str_remove(pattern = "\\?.*") %>%
    enframe(name = "position", value = "subdirectory")

  
  #Description from the example
  bike_desc_tbl <- html_bike_category %>%
    html_nodes('.productTile__productSummaryLeft > meta[itemprop="description"]') %>%
    html_attr("content") %>%
    enframe(name = "position", value = "description")
  
  #Get JSON data
  
  JSON_data_bikes <- html_bike_category %>%
    html_nodes(css = ".productTile") %>%
    html_attr('data-gtm-impression') %>%
    map(fromJSON) %>%
    map(purrr::pluck, 2, "impressions") %>%
    map(na_if, 'not defined') %>%
    map(na_if, "") %>%
    map(~mutate(., across(c("dimension56","price"), as.numeric))) %>%
    bind_rows() %>%
    as_tibble() %>%
    rowid_to_column(var = "position") %>%
    left_join(bike_desc_tbl) %>%
    left_join(url_bikes)
}


# # JSON_data_bikes <- get_data_bikes(url = product_list_link$url[1])
# 
# JSON_data_bikes_tbl <- product_list_link$url %>%
#   map(get_data_bikes) %>% 
#   bind_rows() 
# 
# ## FROM SOL in classbook
# 
# bike_data_cleaned_tbl <- JSON_data_bikes_tbl %>%
#   
#   # Filter for bikes. Only unique ones
#   filter(nchar(.$id) == 4) %>%
#   filter(!(name %>% str_detect("Frameset"))) %>%
#   distinct(id, .keep_all = T) %>%
#   
#   # Split categories (Speedmax had to be treated individually)
#   mutate(category = replace(category, 
#                             name == "Speedmax CF SLX 8.0 SL", "Road/Triathlon Bike/Speedmax")) %>%
#   separate(col = category, into = c("category_1",
#                                     "category_2",
#                                     "category_3"),
#            sep = "(?<!\\s)/(?!\\s)") %>%
#   
#   # Renaming
#   rename("year"       = "dimension50") %>%
#   rename("model"      = "name") %>%
#   rename("gender"     = "dimension63") %>%
#   rename("price_euro" = "metric4") %>%
#   
#   # Fix years manually (have checked the website)
#   mutate(year = replace_na(year, 2021)) %>%
#   
#   # Add frame material
#   mutate(frame_material = case_when(
#     model %>% str_detect(" CF ") ~ "carbon",
#     model %>% str_detect(" CFR ") ~ "carbon",
#     TRUE ~ "aluminium"
#   )
#   ) %>%
#   
#   # Select and order columns
#   select(-c(position, brand, variant, starts_with("dim"), 
#             quantity, feedProductId, price, metric5)) %>%
#   select(id, model, year, frame_material, price_euro, everything())
# 
# saveRDS(bike_data_cleaned_tbl, "./00_data/04_web_scrapped/bike_data_cleaned_tbl.rds")

# bike_data_cleaned_tbl <- readRDS("./00_data/04_web_scrapped/bike_data_cleaned_tbl.rds")
# 
# get_colors <- function(url) {
#   
#   url %>%
#     read_html() %>%
#     html_nodes(css = "script") %>%
#     as.character() %>%
#     str_subset(pattern = "window.deptsfra") %>%
#     str_replace("^[^\\{]+", "") %>%
#     str_replace("[^\\}]+$", "") %>%
#     fromJSON() %>%
#     purrr::pluck("productDetail", "variationAttributes", "values", 1, "value")
# }
# 
# bike_url_vec <- bike_data_cleaned_tbl %>% 
#   pull(subdirectory)
# 
# bike_data_colors_tbl <- bike_data_cleaned_tbl %>%
#   mutate(colors = future_map(bike_url_vec, get_colors))
# 
# saveRDS(bike_data_colors_tbl, "./00_data/04_web_scrapped/bike_data_colors_tbl.rds")


bike_data_colors_tbl <- readRDS("./00_data/04_web_scrapped/bike_data_colors_tbl.rds")

bike_data_colors_tbl <- bike_data_colors_tbl %>%
    unnest(colors) %>%
    mutate(url_color = glue("{url}?dwvar_{id}_pv_rahmenfarbe={colors}")) %>%
  select(-subdirectory) %>%
  mutate(url_color = ifelse(str_detect(colors, pattern = "/"),
                            stringi::stri_replace_last_fixed(url_color, "/", "%2F"),
                            url_color))

# bike_data_colors_tbl %>% glimpse()


get_sizes <- function(url) {
  
  json <- url %>%
    
    read_html() %>%
    html_nodes(css = "script") %>%
    as.character() %>%
    str_subset(pattern = "window.deptsfra") %>%
    str_replace("^[^\\{]+", "") %>%
    str_replace("[^\\}]+$", "") %>%
    fromJSON(flatten = T) %>%
    purrr::pluck("productDetail", "variationAttributes", "values", 2) %>%
    select(id, value, availability.onlyXLeftNumber) %>%
    rename(id_size = id) %>%
    rename(size = value) %>%
    rename(stock_availability = availability.onlyXLeftNumber) %>%
    as_tibble()
  
}

# Pull url vector
bike_url_color_vec <- bike_data_colors_tbl %>% 
  pull(url_color)

test <- get_sizes(bike_url_color_vec[1])

# # Map
# bike_data_sizes_tbl <- bike_data_colors_tbl %>%
#   mutate(size = future_map(bike_url_color_vec, get_sizes))

# # Unnest
# bike_data_sizes_tbl <- bike_data_sizes_tbl %>%
#   unnest(size)
# 
# saveRDS(bike_data_sizes_tbl, "bike_data_sizes_tbl.rds")
















