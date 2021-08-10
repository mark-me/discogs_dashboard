library(httr)		# Library for scraping web data
library(rjson)
library(tidyverse)
library(magrittr)
library(yaml)
library(RSQLite)

config <- read_yaml("config.yml")
source("data-prep/discogs_helper_functions.R")
source("data-prep/load_discogs_collection.R")
source("data-prep/load_discogs_artists.R")

df_collection_items   <- load_discogs_collection()
df_collection_artists <- extract_collection_artists(df_collection_items)
df_collection_labels  <- extract_collection_labels(df_collection_items)
df_collection_formats <- extract_collection_formats(df_collection_items)
df_collection_styles  <- extract_collection_styles(df_collection_items)
df_collection_genres  <- extract_collection_genres(df_collection_items)
df_collection_items   <- clean_collection_item_df(df_collection_items)

# Write collection data to database
db_discogs <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))
dbWriteTable(db_discogs, "collection_items",   df_collection_items,   overwrite = TRUE)
dbWriteTable(db_discogs, "collection_artists", df_collection_artists, overwrite = TRUE)
dbWriteTable(db_discogs, "collection_formats", df_collection_formats, overwrite = TRUE)
dbWriteTable(db_discogs, "collection_labels",  df_collection_labels,  overwrite = TRUE)
dbWriteTable(db_discogs, "collection_genres",  df_collection_genres,  overwrite = TRUE)
dbWriteTable(db_discogs, "collection_styles",  df_collection_styles,  overwrite = TRUE)

# Load artist data for new artists  
df_artists        <- load_discogs_artists(db = db_discogs, df_collection_artists)
if(nrow(df_artists) > 0){
  df_artist_images  <- extract_artist_images(df_artists)
  df_artist_groups  <- extract_artist_groups(df_artists)
  df_artist_aliases <- extract_artist_aliases(df_artists)
  df_artist_members <- extract_artist_members(df_artists)
  df_artist_urls    <- extract_artist_urls(df_artists)
  df_artists        <- clean_artist_df(df_artists)    
  df_artists        <- artists_add_image(df_artists, df_artist_images)

  # Write artist data to database
  name_table <- "artists"
  has_table <- dbExistsTable(db_discogs, name_table)
  dbWriteTable(db_discogs, "artists",        df_artists,        overwrite = !has_table, append = has_table)
  dbWriteTable(db_discogs, "artist_images",  df_artist_images,  overwrite = !has_table, append = has_table)
  dbWriteTable(db_discogs, "artist_groups",  df_artist_groups,  overwrite = !has_table, append = has_table)
  dbWriteTable(db_discogs, "artist_aliases", df_artist_aliases, overwrite = !has_table, append = has_table)
  dbWriteTable(db_discogs, "artist_members", df_artist_members, overwrite = !has_table, append = has_table)
  dbWriteTable(db_discogs, "artist_urls",    df_artist_urls,    overwrite = !has_table, append = has_table)
}

# Load data for new artist releases
if(config$reload_artist_releases_discogs){
  
  df_artists         <- dbReadTable(db_discogs, "artists")
  df_artist_releases <- load_discogs_artist_releases(db, df_artists)

  # Write artist data to database
  name_table <- "artist_releases"
  has_table <- dbExistsTable(db_discogs, name_table)
  dbWriteTable(db_discogs, name_table, df_artist_releases, overwrite = !has_table, append = has_table)
} 

# Read artist data from database
df_artists         <- dbReadTable(db_discogs, "artists")
df_artist_images   <- dbReadTable(db_discogs, "artist_images")
df_artist_groups   <- dbReadTable(db_discogs, "artist_groups")
df_artist_aliases  <- dbReadTable(db_discogs, "artist_aliases")
df_artist_members  <- dbReadTable(db_discogs, "artist_members")
df_artist_urls     <- dbReadTable(db_discogs, "artist_urls")
df_artist_releases <- dbReadTable(db_discogs, "artist_releases")
dbDisconnect(db_discogs) 

