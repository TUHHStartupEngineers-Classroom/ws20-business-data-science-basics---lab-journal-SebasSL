library(RSQLite)
library(tidyverse)

con <- RSQLite::dbConnect(drv    = SQLite(), 
                          dbname = "./00_data/02_chinook/Chinook_Sqlite.sqlite")

# print(dbListTables(con))
# 
# print(tbl(con, "Album"))

album_tbl <- tbl(con, "Album") %>% collect()

# print(album_tbl)

artist_tbl <- dbGetQuery(con, 'SELECT * FROM Artist') %>% collect()

# artist_tbl <- tbl(con, "Artist") %>% collect()


print(artist_tbl)

dbDisconnect(conn = con)