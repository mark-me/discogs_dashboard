library(httr)		# Library for scraping web data
library(rjson)
library(tidyverse)
library(magrittr)
library(yaml)
library(RSQLite)

config <- read_yaml("config.yml")
source("discogs_helper_functions.R")
source("load_discogs_collection.R")
source("load_discogs_artists.R")

if(config$load_collection){
  
  df_collection_items   <- load_discogs_collection()
  df_collection_artists <- extract_collection_artists(df_collection_items)
  df_collection_labels  <- extract_collection_labels(df_collection_items)
  df_collection_formats <- extract_collection_formats(df_collection_items)
  df_collection_styles  <- extract_collection_styles(df_collection_items)
  df_collection_genres  <- extract_collection_genres(df_collection_items)
  df_collection_items   <- remove_collection_item_lists(df_collection_items)
  
  # Write collection data to database
  db_discogs <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))
  dbWriteTable(db_discogs, "collection_items",   df_collection_items,   overwrite = TRUE)
  dbWriteTable(db_discogs, "collection_artists", df_collection_artists, overwrite = TRUE)
  dbWriteTable(db_discogs, "collection_formats", df_collection_formats, overwrite = TRUE)
  dbWriteTable(db_discogs, "collection_labels",  df_collection_labels,  overwrite = TRUE)
  dbWriteTable(db_discogs, "collection_genres",  df_collection_genres,  overwrite = TRUE)
  dbWriteTable(db_discogs, "collection_styles",  df_collection_styles,  overwrite = TRUE)
  dbDisconnect(db_discogs)  
} else {
  
  # Read collection data from database
  db_discogs <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))
  df_collection_items   <- dbReadTable(db_discogs, "collection_items")
  df_collection_artists <- dbReadTable(db_discogs, "collection_artists")
  df_collection_formats <- dbReadTable(db_discogs, "collection_formats")
  df_collection_labels  <- dbReadTable(db_discogs, "collection_labels")
  df_collection_genres  <- dbReadTable(db_discogs, "collection_genres")
  df_collection_styles  <- dbReadTable(db_discogs, "collection_styles")
  dbDisconnect(db_discogs)    
}

if(config$load_artists){
  df_artists <- load_discogs_artists(df_collection_artists)
  df_artist_images  <- extract_artist_images(df_artists)
  df_artist_groups  <- extract_artist_groups(df_artists)
  df_artist_aliases <- extract_artist_aliases(df_artists)
  df_artist_members <- extract_artist_members(df_artists)
  df_artist_urls    <- extract_artist_urls(df_artists)
  df_artists        <- clean_artist_df(df_artists)
  
  # Write artist data to database
  db_discogs <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))
  dbWriteTable(db_discogs, "artists",        df_artists,        overwrite = TRUE)
  dbWriteTable(db_discogs, "artist_images",  df_artist_images,  overwrite = TRUE)
  dbWriteTable(db_discogs, "artist_groups",  df_artist_groups,  overwrite = TRUE)
  dbWriteTable(db_discogs, "artist_aliases", df_artist_aliases, overwrite = TRUE)
  dbWriteTable(db_discogs, "artist_members", df_artist_members, overwrite = TRUE)
  dbWriteTable(db_discogs, "artist_urls",    df_artist_urls,    overwrite = TRUE)
  dbDisconnect(db_discogs)  
} else {
  
  # Read artist data from database
  db_discogs <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))
  df_artists        <- dbReadTable(db_discogs, "artists")
  df_artist_images  <- dbReadTable(db_discogs, "artist_images")
  df_artist_groups  <- dbReadTable(db_discogs, "artist_groups")
  df_artist_aliases <- dbReadTable(db_discogs, "artist_aliases")
  df_artist_members <- dbReadTable(db_discogs, "artist_members")
  df_artist_urls    <- dbReadTable(db_discogs, "artist_urls")
  dbDisconnect(db_discogs) 
}
  

