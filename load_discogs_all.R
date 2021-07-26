library(httr)		# Library for scraping web data
library(rjson)
library(tidyverse)

source("discogs_api_credentials.R")
source("load_discogs_collection.R")

df_collection_items   <- load_discogs_collection()
df_collection_artists <- extract_collection_artists(df_collection_items)
df_collection_labels  <- extract_collection_labels(df_collection_items)
df_collection_formats <- extract_collection_formats(df_collection_items)