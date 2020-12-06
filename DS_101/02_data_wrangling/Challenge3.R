library(vroom)
library(data.table)
library(tidyverse)
col_types_patent <- list(
  id = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_double()
)

patent_tbl <- vroom(
  file       = "./00_data/05_patents/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent,
  na         = c("", "NA", "NULL")
)

#Patent assignee
col_types_patent_assignee <- list(
  patent_id = col_character(),
  assignee_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "./00_data/05_patents/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent_assignee,
  na         = c("", "NA", "NULL")
)

#assignee
col_types_assignee <- list(
  id = col_character(),
  type = col_character(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "./00_data/05_patents/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_assignee,
  na         = c("", "NA", "NULL")
)

#USPC
col_types_uspc <- list(
  patent_id = col_character(),
  mainclass_id = col_character(),
  sequence = col_character()
)

uspc_tbl <- vroom(
  file       = "./00_data/05_patents/uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types_uspc,
  na         = c("", "NA", "NULL")
)

#1.Patent Dominance

patent_dominance_tbl <- assignee_tbl %>%
  rename(assignee_id = id) %>%
  left_join(patent_assignee_tbl) %>%
  group_by(assignee_id,organization) %>%
  summarise("num_patents" = n()) %>%
  arrange(desc(num_patents))

patent_dominance_tbl %>% write_rds("00_data/07_data_journal/patent_dominance_tbl.rds")

#2.Recent patent acitivity

patent_dominance_july_tbl <- assignee_tbl %>%
  rename(assignee_id = id) %>%
  left_join(patent_assignee_tbl) %>%
  rename(id = patent_id) %>%
  left_join(patent_tbl) %>%
  separate(col = date, into = c("year","month","day"),sep = "-") %>%
  mutate(
    year  = as.numeric(year),
    month = as.numeric(month),
    day   = as.numeric(day)
  ) %>%
  select(-year,-day,-id,-num_claims) %>%
  filter(month == 07) %>%
  group_by(assignee_id,organization) %>%
  summarise("num_patents" = n()) %>%
  arrange(desc(num_patents))%>%
  head(10)


patent_dominance_july_tbl %>% write_rds("00_data/07_data_journal/patent_dominance_july_tbl.rds")

#3.Innovation in Tech
##3.1 most innovative tech sector (mainclass with more patents)
most_innovative_sector <- uspc_tbl %>%
  group_by(mainclass_id) %>%
  summarise("num_patents" = n()) %>%
  arrange(desc(num_patents))%>%
  head(10)


most_innovative_sector %>% write_rds("00_data/07_data_journal/most_innovative_sector.rds")

##3.2 top 5 USPTO tech main classes from 10 top companies worldwide

patent_dominance_10_tbl <- assignee_tbl %>%
  rename(assignee_id = id) %>%
  left_join(patent_assignee_tbl) %>%
  group_by(assignee_id,organization) %>%
  summarise("num_patents" = n()) %>%
  arrange(desc(num_patents)) %>%
  head(10)

assignee_id_10 <- patent_dominance_10_tbl$assignee_id

USPTO_top_5_tbl <- assignee_tbl %>%
  rename(assignee_id = id) %>%
  left_join(patent_assignee_tbl) %>%
  left_join(uspc_tbl) %>%
  filter(assignee_id %in% assignee_id_10) %>%
  filter(!is.na(mainclass_id)) %>%
  group_by(mainclass_id) %>%
  summarise("num_patents" = n()) %>%
  arrange(desc(num_patents)) %>%
  head(5)

USPTO_top_5_tbl %>% write_rds("00_data/07_data_journal/USPTO_top_5_tbl.rds")


