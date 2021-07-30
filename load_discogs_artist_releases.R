load_discogs_artist_releases <- function(db, df_artists){
  
  name_table <- "artist_releases"
  
  has_table <- dbExistsTable(db, name_table)
  if(has_table){
    df_previous <- dbReadTable(conn = db, name = name_table)
    df_artists %<>% anti_join(df_previous, by = "id_artist")
  }
  
  lst_releases <- lst_json <- list()
  for(row in 1:nrow(df_artists)){
    
    lst_json <- api_request_artist_release(id_artist = df_artists[row, "id_artist"], 
                                           idx_page = 1, 
                                           api_discogs_config)
    print(paste(row, "-", df_artists[row, "name_artist"]))
    pb <- txtProgressBar(min = 0, max = lst_json$pagination$pages, style = 3)
    for(idx_page in 1:lst_json$pagination$pages){
      
      lst_json <- api_request_artist_release(id_artist = df_artists[row, "id_artist"], 
                                             idx_page = idx_page, 
                                             api_discogs_config)     
      
      for(i in 1:length(lst_json$releases)){
        lst_json$releases[[i]]$stats <- NULL
        lst_json$releases[[i]]$id_artist <- df_artists[row, "id_artist"]
      }
      setTxtProgressBar(pb, idx_page)
      lst_releases <- c(lst_releases, lst_json$releases)
    }
  }  
  
  df_releases <- bind_rows(lst_releases) %>% 
    unique()
  return(df_releases)
}

api_request_artist_release <- function(id_artist, idx_page, api_discogs_config){
 
  lst_json[[1]] <- "You are making requests too quickly."
  while (lst_json[[1]] == "You are making requests too quickly.") {
    
    json <- GET(paste0(api_discogs_config$url, 
                       "/artists/", id_artist, "/releases", 
                       "?page=", idx_page, "&per_page=100",
                       "&token=", api_discogs_config$token))
    json_text <- content(json, as = "text", encoding = "UTF-8")
    lst_json <- fromJSON(json_text)
    if(lst_json[[1]] == "You are making requests too quickly."){
      Sys.sleep(65)
    }
  }
  return(lst_json)
}
