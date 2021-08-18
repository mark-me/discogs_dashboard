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
  
  # Combine into network list data frames
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
    summarise(across(name_artist_real:url_thumbnail, ~ min(!is.na(.x), na.rm= TRUE)),
              qty_collection_items = sum(qty_collection_items, na.rm = TRUE),
              type_performer = min(type_performer, na.rm = TRUE),
              is_active = max(is_active, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(type_node = "performer")
  
  # Add performer roles
  db_conn <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))
  
  res <- dbSendQuery(db_conn, paste0("SELECT id_artist, role, COUNT(*) as qty_roles 
                                      FROM artist_releases
                                      GROUP BY id_artist, role"))
  df_performer_roles <- dbFetch(res)
  
  df_performer_roles %<>% 
    filter(role %in% c("Main", "Producer", "Co-producer", "Mixed by", "Remix"))  

  # Finding the main role of the performer
  df_main_role <- df_performer_roles %>% 
    group_by(id_artist) %>% 
    mutate(qty_role_max = max(qty_roles)) %>% 
    ungroup() %>% 
    filter(qty_roles == qty_role_max) %>% 
    mutate(role = ordered(role, levels = c("Main", "Producer", "Co-producer", "Mixed by", "Remix"))) %>% 
    group_by(id_artist) %>% 
    summarise(role_primary = first(role, order_by = role)) %>% 
    ungroup() %>% 
    mutate(role_primary = as.character(role_primary))
    
  df_performer_roles %<>%   
    mutate(role = recode(role,
                         `Main` = "qty_main_performer", 
                         `Producer` = "qty_producer", 
                         `Co-producer`= "qty_co_producer",
                         `Mixed by` = "qty_mixer",
                         `Remix` = "qty_remixer")) %>%
    pivot_wider(id_cols = "id_artist",
                names_from = role,
                values_from = qty_roles,
                values_fn = sum,
                values_fill = 0) %>% 
    left_join(df_main_role, by = "id_artist") %>% 
    mutate(id_artist = paste0("p_", id_artist))
  
  df_nodes %<>%
    left_join(df_performer_roles, by = c("id_node" = "id_artist"))
  
  # Collect edges
  df_edges <- bind_rows(
    get_membership_edges(),
    get_group_edges(),
    get_alias_edges()
  ) %>% unique()
  
  # Combine into network list data frames
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
    mutate(performer_role = recode(role,
                                   `Main` = "main", 
                                   `Producer` = "producer", 
                                   `Co-producer`= "co_producer",
                                   `Mixed by` = "mixed_by",
                                   `Remix` = "remix")) %>% 
    select(from = id_artist,  to = id_release, performer_role) %>% 
    mutate(type_edge = "release",
           from = paste0("p_", from),
           to = paste0("r_", to))
  
  dbDisconnect(db_conn)
  
  return(df_result)  
}
