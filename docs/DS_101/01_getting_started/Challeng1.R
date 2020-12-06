library(tidyverse)
library(lubridate)

### CHALLENGE

bike_orderlines_wrangled_tbl <- readRDS("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")

#Part 1
salesByStateAndCity <- bike_orderlines_wrangled_tbl %>% 
  select("location","total_price") %>%
  separate(col = "location", into = c("city","state"),sep = ", ") %>%
  group_by(state,city) %>%
  summarize("total_sales" = sum(total_price)) %>%
  mutate(sales_text = scales::dollar(total_sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

salesByState <- bike_orderlines_wrangled_tbl %>% 
  select("location","total_price") %>%
  separate(col = "location", into = c("city","state"),sep = ", ") %>%
  group_by(state) %>%
  summarize("total_sales" = sum(total_price)) %>%
  mutate(sales_text = scales::dollar(total_sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

salesByState %>% write_rds("00_data/01_bike_sales/02_wrangled_data/salesByState.rds")

#PLOT
salesByState %>%
  
  # Setup canvas with the columns year (x-axis) and sales (y-axis)
  ggplot(aes(x = state, y = total_sales)) +
  
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
  ) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#PART 2
salesByStateAndYear <- bike_orderlines_wrangled_tbl %>% 
  select("order_date","location","total_price") %>%
  separate(col = "location", into = c("city","state"),sep = ", ") %>%
  mutate("year" = year(order_date)) %>%
  group_by(state,year) %>%
  summarize("total_sales" = sum(total_price)) %>%
  mutate(sales_text = scales::dollar(total_sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

# print(salesByStateAndYear)

salesByStateAndYear %>% write_rds("00_data/01_bike_sales/02_wrangled_data/salesByStateAndYear.rds")

salesByStateAndYear %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = total_sales, fill = state)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ state) +
  
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


