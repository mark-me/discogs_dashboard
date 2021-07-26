load_discogs_artists <- function(df_collection_artists){

  df_artists <- df_collection_artists %>% 
    select(id_artist = id,
           name_artist = name,
           url_api_artist = resource_url) %>% 
    unique() %>% 
    mutate(url_api_artist = paste0(url_api_artist, "&token=", api_discogs_config$token))
  
  
  json <- GET(paste0(df_artists[1, "url_api_artist"], "?token=", api_discogs_config$token))#lapply(df_artists$url_api_artist, GET)
  json_text <- content(json, as = "text", encoding = "UTF-8")
  lst_collection <- fromJSON(json_text)
  
}


