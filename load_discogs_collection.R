load_discogs_collection <- function(){
  
  lst_collection_releases <- list()
  
  rest_request <- paste0(
    api_discogs_config$url,
    "/users/", api_discogs_config$username,
    "/collection/folders/0/releases?page=1&per_page=100"
  )
  json <- GET(rest_request)
  json_text <- content(json, as = "text", encoding = "UTF-8")
  lst_collection <- fromJSON(json_text)
  
  for(idx_page in 1:lst_collection$pagination$pages){
    
    rest_request <- paste0(
      api_discogs_config$url,
      "/users/", api_discogs_config$username,
      "/collection/folders/0/releases?page=", idx_page, "&per_page=100",
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

extract_collection_artists <- function(df_collection){

  df_release_artists <- extract_sublists_as_df(df_collection, "id_release", "lst_artists")
  df_release_artists %<>%
    select(-anv, -join, -role, -tracks) %>% 
    rename(id_artist = id,
           name_artist = name,
           api_artist = resource_url) %>% 
    mutate(id_artist = as.integer(id_artist))
  
  return(df_release_artists)
}

extract_collection_labels <- function(df_collection){
  
  df_release_labels  <- extract_sublists_as_df(df_collection, "id_release", "lst_labels")
  df_release_labels %<>%
    select(-entity_type, -entity_type_name) %>% 
    rename(id_label = id,
           name_label = name,
           api_label = resource_url) %>% 
    mutate(id_label = as.integer(id_label))
  
  return(df_release_labels)
}

extract_collection_formats <- function(df_collection){
  
  df_release_formats <- extract_sublists_as_df(df_collection, 
                                               colname_id   = "id_release", 
                                               colname_list = "lst_formats")
  
  return(df_release_formats)
}

extract_collection_genres <- function(df_collection){
  
  df_release_genres <- extract_vectors_as_df(df_collection, 
                                             colname_id   = "id_release",
                                             colname_vec  = "vec_genres",
                                             colname_dest = "name_genre")
  return(df_release_genres)
}

extract_collection_styles <- function(df_collection){
  
  df_release_styles <- extract_vectors_as_df(df_collection, 
                                             colname_id   = "id_release",
                                             colname_vec  = "vec_styles",
                                             colname_dest = "name_style")
  return(df_release_styles)
}


clean_collection_item_df <- function(df_collection){
  
  df_collection <- as_tibble(
    df_collection %>% 
      select(-starts_with("lst_")) %>% 
      select(-starts_with("vec_")) %>% 
      mutate(across(everything(), ~ifelse(.x == "NULL", NA, .x)))
    )
  
  df_collection <- as_tibble(sapply(df_collection, unlist))
  
  
  return(df_collection)
}
