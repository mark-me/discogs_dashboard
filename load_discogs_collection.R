library(httr)		# Library for scraping web data
library(rjson)
library(tidyverse)

source("discogs_api_credentials.R")

load_discogs_collection <- function(){
  
  lst_collection_releases <- list()
  
  rest_request <- paste0(
    api_discogs_config$url,
    "/users/",
    api_discogs_config$username,
    "/collection/folders/0/releases?page=1&per_page=100"
  )
  json <- GET(rest_request)
  json_text <- content(json, as = "text", encoding = "UTF-8")
  lst_collection <- fromJSON(json_text)
  
  for(idx_page in 1:lst_collection$pagination$pages){
    
    rest_request <- paste0(
      api_discogs_config$url,
      "/users/",
      api_discogs_config$username,
      "/collection/folders/0/releases?page=",
      idx_page,
      "&per_page=100",
      "&token=", api_discogs_config$token
    )
    json <- GET(rest_request)
    json_text <- content(json, as = "text", encoding = "UTF-8")
    lst_collection <- fromJSON(json_text)
    lst_collection_releases <- c(lst_collection_releases, lst_collection$releases)
    
  }
  rm(lst_collection, json, json_text, rest_request, idx_page)
  
  
  lst_collection_item <- list()
  i <- 0
  for(release in lst_collection_releases) {
    i <- i + 1
    
    lst_collection_item[[i]] <- list(
      id_release  = as.integer(release$id),
      id_instance = as.integer(release$instance_id),
      id_master   = as.integer(release$basic_information$master_id),
      date_added  = release$date_added,
      rating      = release$rating,
      title       = release$basic_information$title,
      year        = release$basic_information$year,
      url_thumb   = release$basic_information$thumb,
      url_cover   = release$basic_information$cover_image, 
      api_master  = release$basic_information$master_url,
      api_release = release$basic_information$resource_url,
      qty_formats = length(release$basic_information$formats),
      lst_formats = release$basic_information$formats,
      qty_labels  = length(release$basic_information$labels),
      lst_labels  = release$basic_information$labels,
      qty_artists = length(release$basic_information$artists),
      lst_artists = release$basic_information$artists,
      qty_genres  = length(release$basic_information$genres),
      vec_genres  = release$basic_information$genres,
      qty_styles  = length(release$basic_information$styles),
      vec_styles  = release$basic_information$styles
    )
  }
  rm(release, i, lst_collection_releases)
  
  df_collection_item <- as_tibble(do.call("rbind", lst_collection_item))
  rm(lst_collection_item)

  return(df_collection_item)  
}

extract_sublists_as_df <- function(df, colname_id, colname_list){
  
  lst_dfs <- list()
  for(row in 1:nrow(df)){
    
    lst <- df[row, colname_list][[1]][[1]]
    lst_dfs[[row]] <- cbind(
      id_release = df[row, colname_id],
      bind_rows(df[row, colname_list])
    )  
  }

  df <- bind_rows(lst_dfs)
  
  return(df)
}

extract_collection_artists <- function(df_collection){

  df_release_artists <- extract_sublists_as_df(df_collection, "id_release", "lst_artists")

  return(df_release_artists)
}

extract_collection_labels <- function(df_collection){
  
  df_release_labels  <- extract_sublists_as_df(df_collection, "id_release", "lst_labels")
  
  return(df_release_labels)
}

extract_collection_formats <- function(df_collection){
  
  df_release_formats <- extract_sublists_as_df(df_collection, "id_release", "lst_formats")
  
  return(df_release_formats)
}

