load_discogs_artists <- function(db, df_collection_artists){

  name_table <- "artists"
  
  df_artists <- df_collection_artists %>% 
    select(id_artist,
           name_artist,
           api_artist) %>% 
    unique() 
  
  has_table <- dbExistsTable(db, name_table)
  if(has_table){
   df_previous <- dbReadTable(conn = db, name = name_table)
   df_artists %<>%
     anti_join(df_previous, by = "id_artist")
  }

  row <- 1
  lst_artists <- list()
  lst_json    <- list()
  while(row <= nrow(df_artists)){
    
    if(df_artists[row, "api_artist"] != "" & !is.na(df_artists[row, "api_artist"])){
      
      json <- GET(paste0(df_artists[row, "api_artist"], 
                         "?token=", api_discogs_config$token))
      json_text <- content(json, as = "text", encoding = "UTF-8")
      lst_json <- fromJSON(json_text)
    }
    
    if(length(lst_json) == 1) {
      print(lst_json[[1]])
      print("Waiting...")
      Sys.sleep(65)
      print("Resuming")
    } else {
      print("Adding new artist")
      lst_data <- list(
        id_artist          = lst_json$id,
        name_artist        = lst_json$name,
        name_artist_real   = lst_json$realname,
        profile            = lst_json$profile,
        vec_urls_artist    = lst_json$urls,
        url_artist_discogs = lst_json$uri,
        api_releases       = lst_json$releases_url,
        lst_images         = lst_json$images,
        lst_groups         = lst_json$groups,
        lst_aliases        = lst_json$aliases,
        lst_members        = lst_json$members
      )
      print(row)
      lst_artists[[row]] <- lst_data
      row <- row + 1
    }
  }
  
  df_artist_information <- as_tibble(do.call("rbind", lst_artists)) %>% 
    filter(id_artist != "NULL")
  
  return(df_artist_information)
}

# Artist images ----
extract_artist_images <- function(df_artists){
  
  df_artist_images <- extract_sublists_as_df(df_artists, 
                                             colname_id   = "id_artist",
                                             colname_list = "lst_images")
  
  df_artist_images %<>% 
    mutate(id_artist = as.character(id_artist)) %>% 
    rename(type_image    = type,
           url_image     = uri,
           url_image_150 = uri150)
  
  return(df_artist_images)
}

# Artist's membership of groups ----
extract_artist_groups <- function(df_artists){
  
  df_artist_groups <- extract_sublists_as_df(df_artists, 
                                             colname_id   = "id_artist",
                                             colname_list = "lst_groups")
  
  df_artist_groups %<>% 
    mutate(id_artist = as.character(id_artist),
           id = as.character(id)) %>% 
    rename(id_group      = id,
           name_group    = name,
           api_group     = resource_url,
           is_active     = active,
           url_thumbnail = thumbnail_url)
  
  return(df_artist_groups)
}

# Artist alliases ----
extract_artist_aliases <- function(df_artists){
  
  df_artist_aliases <- extract_sublists_as_df(df_artists, 
                                              colname_id   = "id_artist",
                                              colname_list = "lst_aliases")
  
  if(nrow(df_artist_aliases) > 0){
    
    df_artist_aliases %<>%
      mutate(id_artist = as.character(id_artist),
             id_alias = as.character(id)) %>% 
      rename(name_alias    = name,
             api_alias     = resource_url,
             url_thumbnail = thumbnail_url)
  }
  
  return(df_artist_aliases)
}

# Active members of bands ----
extract_artist_members <- function(df_artists){
  
  df_artist_members <- extract_sublists_as_df(df_artists, 
                                              colname_id   = "id_artist",
                                              colname_list = "lst_members")
  
  df_artist_members %<>% 
    mutate(id_artist = as.character(id_artist),
           id = as.character(id)) %>% 
    rename(id_member     = id,
           name_member   = name,
           api_member    = resource_url,
           is_active     = active,
           url_thumbnail = thumbnail_url)
  
  return(df_artist_members)
}

# URLs related to the artist ----
extract_artist_urls <- function(df_artists){
  
  df_artist_urls <- extract_vectors_as_df(df_artists, 
                                          colname_id   = "id_artist",
                                          colname_vec  = "vec_urls_artist",
                                          colname_dest = "url_artist")
  
  return(df_artist_urls)
}

# Remove lists and vectors from data-frame and remove NULL's to make it a regular data frame ----
clean_artist_df <- function(df_artists){
  
  df_artists <- as_tibble(
    df_artists %>% 
      select(-starts_with("lst_")) %>% 
      select(-starts_with("vec_")) %>% 
      mutate(across(everything(), ~ifelse(.x == "NULL", NA, .x)))
  )
  
  df_artists <- as_tibble(sapply(df_artists, unlist))
  
  
  return(df_artists)
}

artists_add_image <- function(df_artists, df_artist_images){
  
  df_image <- df_artists %>% 
    inner_join(df_artist_images, by = "id_artist") %>%
    arrange(id_artist, type_image) %>% 
    group_by(id_artist) %>% 
    summarise(url_image = first(url_image),
              url_thumbnail = first(url_image_150)) %>% 
    ungroup() 
  
  df_artists %<>%
    left_join(df_image, by = "id_artist")
  
  return(df_artists)
}
