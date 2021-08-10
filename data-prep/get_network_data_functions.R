# Networks ----
get_release_network <- function(){

  nw_performers <- get_performer_network()
  
  df_nodes <- bind_rows(nw_performers$df_nodes, 
                        get_master_nodes() %>% mutate(type_node = "release"))

  df_edges <- bind_rows(nw_performers$df_edges, 
                        get_master_edges())
  
  df_edges %<>% 
    filter(from %in% df_nodes$id_node)
  
  # Find the number of releases per performer
  cols_nodes <- names(df_nodes)
  df_nodes %<>% 
    left_join(select(df_edges, from, type_edge) 
              %>% filter(type_edge == "release"),
              by = c("id_node" = "from")) %>% 
    mutate(is_release = as.integer(type_edge == "release")) %>% 
    group_by(across(one_of(cols_nodes))) %>% 
    summarise(qty_releases = sum(is_release, na.rm = TRUE)) %>% 
    ungroup()
  
  lst_network <- list(
    df_nodes = df_nodes,
    df_edges = df_edges
  )
  
  return(lst_network)
}

get_collection_network <- function(){
  
  df_nodes <- bind_rows(get_performer_nodes() %>% mutate(type_node = "performer"),
                        get_item_nodes() %>% mutate(type_node = "collection_item"))
  df_edges <- bind_rows(get_performer_edges(),
                        get_collection_item_edges()) %>% 
    filter(from %in% df_nodes$id_node)
  
  # Find the number of releases per performer
  cols_nodes <- names(df_nodes)
  df_nodes %<>% 
    left_join(select(df_edges, from, type_edge) 
               %>% filter(type_edge == "release"),
              by = c("id_node" = "from")) %>% 
    mutate(is_release = as.integer(type_edge == "release")) %>% 
    group_by(across(one_of(cols_nodes))) %>% 
    summarise(qty_releases = sum(is_release, na.rm = TRUE)) %>% 
    ungroup()
  
  lst_network <- list(
    df_nodes = df_nodes,
    df_edges = df_edges
  )
  
  return(lst_network)
}

get_performer_network <- function(){
  
  df_nodes <- bind_rows(
    get_artist_nodes(),
    get_member_nodes(),
    get_group_nodes(),
    get_alias_nodes()
  )
  
  df_nodes %<>%
    group_by(id_node, name_node) %>% 
    summarise(across(name_artist_real:is_active, ~ min(.x, na.rm= TRUE))) %>% 
    ungroup() %>% 
    mutate(type_node = "performer")
  
  df_edges <- bind_rows(
    get_membership_edges(),
    get_group_edges(),
    get_alias_edges()
  ) %>% unique()
  
  lst_network <- list(
    df_nodes = df_nodes,
    df_edges = df_edges
  )
  
  return(lst_network)  
}

# Nodes ----
get_artist_nodes <- function(){
  
  db_conn <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))
  
  res <- dbSendQuery(db_conn, paste0("SELECT * FROM artists"))
  df_artist_result <- dbFetch(res)
  
  res <- dbSendQuery(db_conn, paste0("SELECT id_artist, count(*) as qty_collection_items 
                                   FROM collection_artists
                                   GROUP BY id_artist"))
  df_collection_result <- dbFetch(res)
  
  df_artist_result %<>% 
    left_join(df_collection_result, by = "id_artist") %>% 
    select(id_node = id_artist, name_node = name_artist, everything()) %>% 
    mutate(type_performer = "artist",
           id_node = paste0("p_", id_node))
  
  dbDisconnect(db_conn)
  
  return(df_artist_result)  
}

get_member_nodes <- function(){
  
  db_conn <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))
  
  res <- dbSendQuery(db_conn, paste0("SELECT * FROM artist_members"))
  df_result <- dbFetch(res)
  
  df_result %<>% 
    select(-id_artist, -api_member) %>% 
    select(id_node = id_member, name_node = name_member, everything()) %>% 
    mutate(type_performer = "member",
           id_node = paste0("p_", id_node))
  
  dbDisconnect(db_conn)
  
  return(df_result)  
}

get_group_nodes <- function(){
  
  db_conn <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))
  
  res <- dbSendQuery(db_conn, paste0("SELECT * FROM artist_groups"))
  df_result <- dbFetch(res)
  
  df_result %<>% 
    select(-id_artist, -api_group) %>% 
    select(id_node = id_group, name_node = name_group, everything()) %>% 
    mutate(type_performer = "member",
           id_node = paste0("p_", id_node))
  
  dbDisconnect(db_conn)
  
  return(df_result)  
}

get_alias_nodes <- function(){
  
  db_conn <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))
  
  res <- dbSendQuery(db_conn, paste0("SELECT * FROM artist_aliases"))
  df_result <- dbFetch(res)
  
  df_result %<>% 
    select(-id_artist, -api_alias) %>% 
    select(id_node = id_alias, name_node = name_alias, everything()) %>% 
    mutate(type_performer = "alias",
           id_node = paste0("p_", id_node))
  
  dbDisconnect(db_conn)
  
  return(df_result)  
}

get_collection_item_nodes <- function(){
  
  db_conn <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))
  
  res <- dbSendQuery(db_conn, paste0("SELECT * FROM collection_items"))
  df_result <- dbFetch(res)
  
  df_result %<>% 
    select(-id_instance, -starts_with("api_"), -starts_with("qty_")) %>% 
    select(id_node = id_release, name_node = title, url_image = url_cover, everything()) %>% 
    mutate(type_release = "collection_item",
           id_node = paste0("r_", id_node))
  
  dbDisconnect(db_conn)
  
  return(df_result)  
}

get_master_nodes <- function(){
  
  db_conn <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))
  
  res <- dbSendQuery(db_conn, paste0("SELECT artist_releases.*, collection_items.id_instance 
                                      FROM artist_releases
                                      LEFT JOIN collection_items
                                        ON artist_releases.id_release = collection_items.id_master"))
  df_result <- dbFetch(res)
  
  df_result %<>% 
    filter(role %in% c("Main", "Producer", "Co-producer", "Mixed by", "Remix")) %>% 
    mutate(in_collection = !is.na(id_instance)) %>% 
    rename(id_node = id_release) %>% 
    group_by(id_node) %>% 
    summarise(name_node = first(title, order_by = year) ,
              year = min(year, na.rm = TRUE),
              in_collection = max(in_collection),
              url_thumbnail = first(image_thumbnail, order_by = year)) %>% 
    ungroup() %>% 
    mutate(type_release = "release",
           id_node = paste0("r_", id_node))  
  dbDisconnect(db_conn)
  
  return(df_result)  
}

# Edges ----
get_membership_edges <- function(){
  
  db_conn <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))
  
  res <- dbSendQuery(db_conn, paste0("SELECT id_artist, id_member, is_active FROM artist_members"))
  df_result <- dbFetch(res)
  
  df_result %<>% 
    select(from = id_artist,  to = id_member, everything()) %>% 
    mutate(type_edge = "membership",
           from = paste0("p_", from),
           to = paste0("p_", to))
  
  dbDisconnect(db_conn)
  
  return(df_result)  
}

get_group_edges <- function(){
  
  db_conn <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))
  
  res <- dbSendQuery(db_conn, paste0("SELECT id_artist, id_group, is_active FROM artist_groups"))
  df_result <- dbFetch(res)
  
  df_result %<>% 
    select(from = id_artist,  to = id_group, everything()) %>% 
    mutate(type_edge = "membership",
           from = paste0("p_", from),
           to = paste0("p_", to))
  
  dbDisconnect(db_conn)
  
  return(df_result)  
}

get_alias_edges <- function(){
  
  db_conn <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))
  
  res <- dbSendQuery(db_conn, paste0("SELECT id_artist, id_alias FROM artist_aliases"))
  df_result <- dbFetch(res)
  
  df_result %<>% 
    select(from = id_artist,  to = id_alias) %>% 
    mutate(type_edge = "alias",
           from = paste0("p_", from),
           to = paste0("p_", to))
  
  dbDisconnect(db_conn)
  
  return(df_result)  
}

get_collection_item_edges <- function(){
  
  db_conn <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))
  
  res <- dbSendQuery(db_conn, paste0("SELECT id_artist, id_release FROM collection_artists"))
  df_result <- dbFetch(res)
  
  df_result %<>% 
    select(from = id_artist,  to = id_release) %>% 
    mutate(type_edge = "release",
           from = paste0("p_", from),
           to = paste0("r_", to))
  
  dbDisconnect(db_conn)
  
  return(df_result)  
}

get_master_edges <- function(){
  
  db_conn <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))
  
  res <- dbSendQuery(db_conn, paste0("SELECT DISTINCT id_artist, id_release, role
                                     FROM artist_releases"))
  df_result <- dbFetch(res)
  
  df_result %<>% 
    filter(role %in% c("Main", "Producer", "Co-producer", "Mixed by", "Remix")) %>% 
    select(from = id_artist,  to = id_release) %>% 
    mutate(type_edge = "release",
           from = paste0("p_", from),
           to = paste0("r_", to))
  
  dbDisconnect(db_conn)
  
  return(df_result)  
}
