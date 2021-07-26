library(httr)		# Library for scraping web data
library(rjson)
library(tidyverse)
library(magrittr)
library(yaml)
library(RSQLite)

config <- read_yaml("config.yml")
source("discogs_api_credentials.R")
source("load_discogs_collection.R")

df_collection_items   <- load_discogs_collection()
df_collection_artists <- extract_collection_artists(df_collection_items)
df_collection_labels  <- extract_collection_labels(df_collection_items)
df_collection_formats <- extract_collection_formats(df_collection_items)
df_collection_items   <- remove_collection_item_lists(df_collection_items)

# Write collection data to database
db_discogs <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))
dbWriteTable(db_discogs, "collection_items",   df_collection_items,   overwrite = TRUE)
dbWriteTable(db_discogs, "collection_artists", df_collection_artists, overwrite = TRUE)
dbWriteTable(db_discogs, "collection_formats", df_collection_formats, overwrite = TRUE)
dbWriteTable(db_discogs, "collection_labels",  df_collection_labels,  overwrite = TRUE)

dbDisconnect(db_discogs)
