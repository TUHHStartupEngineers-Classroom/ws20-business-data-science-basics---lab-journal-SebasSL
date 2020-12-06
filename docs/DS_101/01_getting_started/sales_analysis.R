# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(lubridate)
library("writexl")
# 2.0 Importing Files ----7
path <- "./00_data/01_bike_sales/01_raw_data/"
bikesInfo <- readxl::read_excel(paste(path,"bikes.xlsx",sep = ""))
bikesShops <-readxl::read_excel(paste(path,"bikeshops.xlsx",sep = ""))
orderLines <- readxl::read_excel(paste(path,"orderlines.xlsx",sep = ""))

# 3.0 Examining Data ----
# print(bikesInfo %>% head(n = 5))
# print(bikesShops %>% head(n = 5))
# print(orderLines %>% head(n = 5))
# glimpse(bikesInfo)
# glimpse(bikesShops)
# glimpse(orderLines)

# 4.0 Joining Data ----
bike_orderlines_joined_tbl <- orderLines %>% left_join(bikesInfo,by = c("product.id"="bike.id")) %>%
  left_join(bikesShops,by = c("customer.id"="bikeshop.id"))

# glimpse(bike_orderlines_joined_tbl)


# 5.0 Wrangling Data ----


bike_orderlines_joined_tbl <- bike_orderlines_joined_tbl %>% 
  separate(col = "category", into = c("category.1","category.2","category.3"), sep = " - ") %>%
  mutate("total.price" = price * quantity)

# glimpse(bike_orderlines_joined_tbl)

## part added by the solution

bike_orderlines_joined_tbl <- bike_orderlines_joined_tbl %>% select(-...1,-gender) %>%
  select(-ends_with(".id")) %>% 
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>%
  select("order.id", contains("order"),contains("model"),contains("category"),"price","quantity", 
         "total.price", everything()) %>%
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.","_"))

# glimpse(bike_orderlines_joined_tbl)

# 6.0 Business Insights ----
# 6.1 Sales by Year ----

# Step 1 - Manipulate

salesByYear <- bike_orderlines_joined_tbl %>% 
  select("order_date","total_price") %>%
  mutate("year" = year(order_date)) %>%
  group_by(year) %>%
  summarize("total_sales" = sum(total_price)) %>%
  mutate(sales_text = scales::dollar(total_sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

# print(salesByYear)


# Step 2 - Visualize

### FROM SOL CODE
salesByYear %>%
  
  # Setup canvas with the columns year (x-axis) and sales (y-axis)
  ggplot(aes(x = year, y = total_sales)) +
  
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = sales_text)) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
  # Formatting
  # scale_y_continuous(labels = scales::dollar) + # Change the y-axis. 
  # Again, we have to adjust it for euro values
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by year",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )

# 6.2 Sales by Year and Category 2 ----

# Step 1 - Manipulate

salesByYearAndCategory <- bike_orderlines_joined_tbl %>% 
  select("order_date","total_price",contains("category")) %>%
  mutate("year" = year(order_date)) %>%
  group_by(year,category_1) %>%
  summarize("total_sales" = sum(total_price)) %>%
  mutate(sales_text = scales::dollar(total_sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

# print(salesByYearAndCategory)

# Step 2 - Visualize

salesByYearAndCategory %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = total_sales, fill = category_1)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ category_1) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )


### CHALLENGE

salesByStateAndCity <- bike_orderlines_joined_tbl %>% 
  select("location","total_price") %>%
  separate(col = "location", into = c("city","state"),sep = ", ") %>%
  group_by(state,city) %>%
  summarize("total_sales" = sum(total_price)) %>%
  mutate(sales_text = scales::dollar(total_sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

# print(salesByStateAndCity)

salesByState <- bike_orderlines_joined_tbl %>% 
  select("location","total_price") %>%
  separate(col = "location", into = c("city","state"),sep = ", ") %>%
  group_by(state) %>%
  summarize("total_sales" = sum(total_price)) %>%
  mutate(sales_text = scales::dollar(total_sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

# print(salesByState)

salesByStateAndYear <- bike_orderlines_joined_tbl %>% 
  select("order_date","location","total_price") %>%
  separate(col = "location", into = c("city","state"),sep = ", ") %>%
  mutate("year" = year(order_date)) %>%
  group_by(state,year) %>%
  summarize("total_sales" = sum(total_price)) %>%
  mutate(sales_text = scales::dollar(total_sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

print(salesByStateAndYear)

### END CHALLENGE

# 7.0 Writing Files ----

# 7.0 Writing Files ----

# 7.1 Excel ----
# bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl
# 
# 
# bike_orderlines_wrangled_tbl %>%
#   write_xlsx("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")
# 
# # 7.2 CSV ----
# bike_orderlines_wrangled_tbl %>% 
#   write_csv("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")
# 
# # 7.3 RDS ----
# bike_orderlines_wrangled_tbl %>% 
#   write_rds("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")
