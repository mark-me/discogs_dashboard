load_discogs_artist_releases <- function(df_artists){
  
  name_table <- "artist_releases"
  
  has_table <- dbExistsTable(db, name_table)
  if(has_table){
    df_previous <- dbReadTable(conn = db, name = name_table)
    df_artists %<>% anti_join(df_previous, by = "id_artist")
  }
  
  # Gathering artist release JSON replies in a list
  lst_releases <- lst_json <- list()
  for(row in 1:nrow(df_artists)){
    
    # Get the number of API pages
    lst_json <- api_request_artist_release(id_artist = df_artists[row, "id_artist"], 
                                           idx_page = 1, 
                                           api_discogs_config)
    print(paste(row, "-", df_artists[row, "name_artist"]))

    # Make sure the artist is found on Discogs
    if(is.null(lst_json$message)){  
      pb <- txtProgressBar(min = 0, max = lst_json$pagination$pages, style = 3)
      
      # Iterate through release pages 
      for(idx_page in 1:lst_json$pagination$pages){
        
        lst_json <- api_request_artist_release(id_artist = df_artists[row, "id_artist"], 
                                               idx_page = idx_page, 
                                               api_discogs_config)     
        # Check whether the artist was found
        
        # Add found artist information to the list
        for(i in 1:length(lst_json$releases)){
          lst_json$releases[[i]]$stats <- NULL
          lst_json$releases[[i]]$id_artist <- df_artists[row, "id_artist"]
        }
        setTxtProgressBar(pb, idx_page)
        lst_releases <- c(lst_releases, lst_json$releases)
      }
    }
  }  
  
  # Combine artist release info from the list into a data frame
  df_releases <- bind_rows(lst_releases) %>% 
    rename(id_release = id,
           id_release_main = main_release,
           api_release = resource_url,
           image_thumbnail = thumb) %>% 
    mutate(id_release = as.character(id_release),
           id_release_main = as.character(id_release_main)) %>% 
    unique()
  
  return(df_releases)
}

api_request_artist_release <- function(id_artist, idx_page, api_discogs_config){
 
  # Checking if there were too many discogs API requests
  lst_json[[1]] <- "You are making requests too quickly."
  while (lst_json[[1]] == "You are making requests too quickly.") {
    
    json <- GET(paste0(api_discogs_config$url, 
                       "/artists/", id_artist, "/releases", 
                       "?page=", idx_page, "&per_page=100",
                       "&token=", api_discogs_config$token))
    json_text <- content(json, as = "text", encoding = "UTF-8")
    lst_json <- fromJSON(json_text)
    
    # Give Discogs API some breathing space
    if(lst_json[[1]] == "You are making requests too quickly."){
      Sys.sleep(65)
    }
  }
  return(lst_json)
}

