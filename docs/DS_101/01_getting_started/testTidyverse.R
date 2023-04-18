library(tidyverse);
### CREATE TIBBLE
print(tibble(
  x = 1:50,
  y = runif(50),
  z = x + y^2,
  outcome = rnorm(50)
)
)

### PIVOT LONGER
# diamonds2 <- readRDS("./02_data_wrangling/diamonds2.rds")
#
# print(diamonds2 %>% head(n=5))
# 
# print(diamonds2 %>% 
#         pivot_longer(cols      = c("2008", "2009"), 
#                      names_to  = 'year', 
#                      values_to = 'price') %>% 
#         head(n = 6))

### PIVOT WIDER
# diamonds3 <- readRDS("./02_data_wrangling/diamonds3.rds")
# 
# diamonds3 %>% head(n = 5)
# print(diamonds3 %>% pivot_wider(names_from = "dimension", values_from = "measurement"))


### SEPARATE
# diamonds4 <- readRDS("./02_data_wrangling/diamonds4.rds")
# 
# print(diamonds4)
# 
# print(diamonds4 %>% separate(col = "dim", into = c("dim1", "dim2", "dim3"), sep = "/", convert = T, remove = TRUE))


### UNITE
# diamonds5 <- readRDS("./02_data_wrangling/diamonds5.rds")
# 
# print(diamonds5)
# 
# print(diamonds5 %>% unite(col = "clarity", c("clarity_prefix","clarity_suffix"), sep = " "))


