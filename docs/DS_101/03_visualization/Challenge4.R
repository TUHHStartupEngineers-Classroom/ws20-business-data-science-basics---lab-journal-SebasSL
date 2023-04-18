library(tidyverse)
# covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
# 
# covid_data_tbl %>% write_rds("00_data/06_covid/covid_data_tbl.rds")

covid_data_tbl <- read_rds("00_data/06_covid/covid_data_tbl.rds")

#PART 1
covid_data_tbl <- covid_data_tbl %>%
  rename(Country = countriesAndTerritories, Continent = continentExp)

filtered_covid_data_tbl <- covid_data_tbl %>%
  filter(Country %in% c("Germany","United_Kingdom","France","Spain","United_States_of_America")) %>%
  arrange(month) %>%
  arrange(Country) %>%
  select(Country,month,cases) %>%
  group_by(Country) %>%
  mutate(cum_cases = cumsum(cases)) %>%
  mutate(month_name = month.name[month]) %>%
  mutate(cum_days = 1:n())

filtered_covid_data_tbl %>% write_rds("00_data/07_data_journal/filtered_covid_data_tbl.rds")

month_arr <- c(31,60,91,121,152,182,213,244,274,305,335)

filtered_covid_data_tbl %>%
  ggplot(aes(cum_days, cum_cases, color = Country)) +
  geom_line( size = 1) +
  scale_y_continuous(name = "Cumulative Cases" , limits = c(0,15e6), labels = scales::unit_format(scale = 1e-6,
                                                    preix = "",
                                                    suffix = "M")) +
  scale_x_continuous(name = "Year 2020", breaks = month_arr, labels = month.name[1:11]) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "bottom"
  ) +
  labs(
    title = str_glue("COVID-19 confirmed cases worldwide")
  )

#PART 2

covid_data_tbl <- covid_data_tbl %>% 
  mutate(across(Country, str_replace_all, "_", " ")) %>%
  mutate(Country = case_when(
    
    Country == "United Kingdom" ~ "UK",
    Country == "United States of America" ~ "USA",
    Country == "Czechia" ~ "Czech Republic",
    TRUE ~ Country
    
  ))

world <- map_data("world")

mortality_rate_tbl <- covid_data_tbl %>%
  select(Country,cases,deaths,geoId,countryterritoryCode,popData2019,Continent) %>%
  group_by(Country,geoId,countryterritoryCode,popData2019,Continent) %>%
  summarise("total_deaths" = sum(deaths)) %>%
  mutate(mort_rate = total_deaths/popData2019) %>%
  mutate(mort_rate_percentage = scales::percent(mort_rate, accuracy = 1e-3)) %>%
  rename(region = Country)

mortality_rate_tbl %>% write_rds("00_data/07_data_journal/mortality_rate_tbl.rds")

plot_data <- world %>%
  left_join(mortality_rate_tbl)

ggplot(plot_data, aes(map_id = region, fill = mort_rate))+
  geom_map(map = plot_data,  color = "#AAAAAA")+
  expand_limits(x = c(180,-180), y = c(85,-85)) +
  scale_fill_continuous(name = "Mortality Rate", 
                        low = "#DD0000", 
                        high = "#440000",
                        na.value = "grey",
                        limits = c(0,0.0015),
                        breaks = c(0,0.0003,0.0006,0.0009,0.0012,0.0015),
                        labels = scales::percent
                        ) +
  theme(
    axis.title = element_blank() ,
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(
    title = str_glue("Confirmed COVID-19 deaths relative to the size of population")
  )





  
  