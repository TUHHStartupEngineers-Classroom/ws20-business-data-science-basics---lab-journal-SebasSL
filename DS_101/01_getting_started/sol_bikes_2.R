# WEBSCRAPING ----

# 1.0 LIBRARIES ----

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing

# 1.1 COLLECT PRODUCT FAMILIES ----

url_home          <- "https://www.canyon.com/en-de"
# xopen(url_home) # Open links directly from RStudio to inspect them

# Read in the HTML for the entire webpage
html_home         <- read_html(url_home)

# Web scrape the ids for the families
bike_family_tbl <- html_home %>%
  
  # Get the nodes for the families ...
  html_nodes(css = ".js-navigationDrawer__list--secondary") %>%
  # ...and extract the information of the id attribute
  html_attr('id') %>%
  
  # Remove the product families Gear and Outlet and Woman 
  # (because the female bikes are also listed with the others)
  discard(.p = ~stringr::str_detect(.x,"WMN|WOMEN|GEAR|OUTLET")) %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "family_class") %>%
  
  # Add a hashtag so we can get nodes of the categories by id (#)
  mutate(
    family_id = str_glue("#{family_class}")
  )

# 1.2 COLLECT PRODUCT CATEGORIES ----

# Combine all Ids to one string so that we will get all nodes at once
# (seperated by the OR operator ",")
family_id_css <- bike_family_tbl %>%
  pull(family_id) %>%
  stringr::str_c(collapse = ", ")
family_id_css
## "#js-navigationList-ROAD, #js-navigationList-MOUNTAIN, #js-navigationList-EBIKES, #js-navigationList-HYBRID-CITY, #js-navigationList-YOUNGHEROES"

# Extract the urls from the href attribute
bike_category_tbl <- html_home %>%
  
  # Select nodes by the ids
  html_nodes(css = family_id_css) %>%
  
  # Going further down the tree and select nodes by class
  # Selecting two classes makes it specific enough
  html_nodes(css = ".navigationListSecondary__listItem .js-ridestyles") %>%
  html_attr('href') %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "subdirectory") %>%
  
  # Add the domain, because we will get only the subdirectories
  mutate(
    url = glue("https://www.canyon.com{subdirectory}")
  ) %>%
  
  # Some categories are listed multiple times.
  # We only need unique values
  distinct(url)


# 2.0 COLLECT BIKE DATA ----

# 2.1 Get URL for each bike of the Product categories

# select first bike category url
# bike_category_url <- bike_category_tbl$url[1]
# 
# # Alternatives for selecting values
# # bike_category_url <- bike_category_tbl %$% url %>% .[1]
# # bike_category_url <- bike_category_tbl %>% pull(url) %>% .[1]
# # bike_category_url <- deframe(bike_category_tbl[1,])
# # bike_category_url <- bike_category_tbl %>% first %>% first
# 
# xopen(bike_category_url)
# 
# # Get the URLs for the bikes of the first category
# html_bike_category  <- read_html(bike_category_url)
# bike_url_tbl        <- html_bike_category %>%
#   
#   # Get the 'a' nodes, which are hierarchally underneath 
#   # the class productTile__contentWrapper
#   html_nodes(css = ".productTile__contentWrapper > a") %>%
#   html_attr("href") %>%
#   
#   # Remove the query parameters of the URL (everything after the '?')
#   str_remove(pattern = "\\?.*") %>%
#   
#   # Convert vector to tibble
#   enframe(name = "position", value = "url")
# 
# # 2.1.2 Extract the descriptions (since we have retrieved the data already)
# bike_desc_tbl <- html_bike_category %>%
#   
#   # Get the nodes in the meta tag where the attribute itemprop equals description
#   html_nodes('.productTile__productSummaryLeft > meta[itemprop="description"]') %>%
#   
#   # Extract the content of the attribute content
#   html_attr("content") %>%
#   
#   # Convert vector to tibble
#   enframe(name = "position", value = "description")
# 
# 
# # 2.1.3 Get even more data from JSON files
# bike_json_tbl  <- html_bike_category %>%
#   
#   html_nodes(css = '.productGrid__listItem.xlt-producttile > div') %>%
#   html_attr("data-gtm-impression") %>%
#   
#   # Convert the JSON format to dataframe
#   # map runs that function on each element of the list
#   map(fromJSON) %>% # need JSON ### need lists
#   
#   # Extract relevant information of the nested list
#   map(purrr::pluck, 2, "impressions") %>% # Need purrr and expl above
#   
#   # Set "not defined" and emtpy fields to NA (will be easier to work with)
#   map(na_if, "not defined") %>%
#   map(na_if, "") %>%
#   
#   # The class of dimension56 and price varies between numeric and char.
#   # This converts this column in each list to numeric
#   # across allows to perform the same operation on multiple columns
#   map(~mutate(., across(c("dimension56","price"), as.numeric))) %>%
#   
#   # Stack all lists together
#   bind_rows() %>%
#   # Convert to tibble so that we have the same data format
#   as_tibble() %>%
#   
#   # Add consecutive numbers so that we can bind all data together
#   # You could have also just use bind_cols()
#   rowid_to_column(var='position') %>%
#   left_join(bike_desc_tbl) %>%
#   left_join(bike_url_tbl)


get_bike_data <- function(url) {
  
  html_bike_category <- read_html(url)
  
  # Get the URLs
  bike_url_tbl  <- html_bike_category %>%
    html_nodes(css = ".productTile__contentWrapper > a") %>%
    html_attr("href") %>%
    str_remove(pattern = "\\?.*") %>%
    enframe(name = "position", value = "url")
  
  # Get the descriptions
  bike_desc_tbl <- html_bike_category %>%
    html_nodes(css = '.productTile__productSummaryLeft > 
                      meta[itemprop="description"]') %>%
    html_attr("content") %>%
    enframe(name = "position", value = "description")
  
  # Get JSON data
  bike_json_tbl <- html_bike_category %>%
    html_nodes(css = '.productGrid__listItem.xlt-producttile > div') %>%
    html_attr("data-gtm-impression") %>%
    map(fromJSON) %>% # need JSON ### need lists
    map(purrr::pluck, 2, "impressions") %>% 
    map(na_if, "not defined") %>%
    map(na_if, "") %>%
    map(~mutate(., across(c("dimension56","price"), as.numeric))) %>%
    bind_rows() %>%
    as_tibble() %>%
    rowid_to_column(var='position') %>%
    left_join(bike_desc_tbl) %>%
    left_join(bike_url_tbl)
}

# # 2.3.1a Map the function against all urls
# 
# # Extract the urls as a character vector
# bike_category_url_vec <- bike_category_tbl %>% 
#   pull(url)
# 
# # Run the function with every url as an argument
# bike_data_lst <- map(bike_category_url_vec, get_bike_data)
# 
# # Merge the list into a tibble
# bike_data_tbl <- bind_rows(bike_data_lst)
# 
# # 
# # bike_data_tbl<- bike_data_tbl %>%
# #   group_by(id) %>%
# #   filter(n()>1) %>%
# #   arrange(id) 
# 
# bike_data_cleaned_tbl <- bike_data_tbl %>%
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


view(bike_data_cleaned_tbl)
