create_nodes_edges <- function(file_db){
  
  create_performer_nodes(file_db)
  create_performer_edges(file_db)
  create_master_nodes(file_db)
  create_master_edges(file_db)
  create_collection_item_nodes(file_db)
  create_collection_item_edges(file_db)
}

# Performer nodes ----
create_performer_nodes <- function(file_db){
  
  name_table <- "nodes_performers"
  db <- dbConnect(RSQLite::SQLite(), file_db)
  has_table <- dbExistsTable(db, name_table)
  
  # Artists
  res <- dbSendQuery(db, paste0("SELECT id_artist, count(*) as qty_collection_items 
                                 FROM collection_artists
                                 GROUP BY id_artist"))
  df_collection_result <- dbFetch(res)
  
  df_artists <- dbReadTable(db, "artists") %>% 
    left_join(df_collection_result, by = "id_artist") %>% 
    select(id_node = id_artist, name_node = name_artist, everything()) %>% 
    mutate(type_performer = "artist",
           id_original = id_node,
           id_node = paste0("p_", id_node))
  
  # Members
  df_members <- dbReadTable(db, "artist_members")%>% 
    select(-id_artist, -api_member) %>% 
    select(id_node = id_member, name_node = name_member, everything()) %>% 
    mutate(type_performer = "member",
           id_original = id_node,
           id_node = paste0("p_", id_node))
  
  # Groups
  df_groups <- dbReadTable(db, "artist_groups") %>%  
    select(-id_artist, -api_group) %>% 
    select(id_node = id_group, name_node = name_group, everything()) %>% 
    mutate(type_performer = "member",
           id_original = id_node,
           id_node = paste0("p_", id_node))
  
  # Aliases
  df_aliases <- dbReadTable(db, "artist_aliases") %>% 
    select(-id_artist, -api_alias) %>% 
    select(id_node = id_alias, name_node = name_alias, everything()) %>% 
    mutate(type_performer = "alias",
           id_original = id_node,
           id_node = paste0("p_", id_node))
  
  # Combine all nodes
  df_nodes <- bind_rows(df_artists, df_members, df_groups, df_aliases)
  
  df_nodes %<>%
    mutate(is_active = ifelse(is.na(is_active), TRUE, is_active)) %>% 
    group_by(id_node, id_original, name_node) %>% 
    summarise(across(name_artist_real:url_thumbnail, ~ first(.x, order_by = .x)),
              qty_collection_items = sum(qty_collection_items, na.rm = TRUE),
              type_performer = min(type_performer, na.rm = TRUE),
              is_active = max(is_active, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(type_node = "performer")
  
  # Derive performer roles
  df_roles <- get_performer_roles(file_db)
  
  df_nodes %<>%
    left_join(df_roles, by = c("id_node" = "id_artist"))
  
  dbWriteTable(db, name_table, df_nodes, overwrite = has_table) 
  dbDisconnect(db)
  
  return(df_nodes)  
}

get_performer_roles <- function(file_db){
  
  # Add performer roles
  db <- dbConnect(RSQLite::SQLite(), file_db)
  
  res <- dbSendQuery(db, paste0("SELECT id_artist, role, COUNT(*) as qty_roles 
                                      FROM artist_releases
                                      GROUP BY id_artist, role"))
  
  df_performer_roles <- dbFetch(res)%>% 
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
  
  return(df_performer_roles)
}

create_performer_edges <- function(file_db){
  
  name_table <- "edges_performers"
  db <- dbConnect(RSQLite::SQLite(), file_db)
  has_table <- dbExistsTable(db, name_table)

  # Artist - membership edges
  res <- dbSendQuery(db, paste0("SELECT id_artist, id_member, is_active FROM artist_members"))
  df_members <- dbFetch(res) %>%  
    select(from = id_artist,  to = id_member, everything()) %>% 
    mutate(type_edge = "membership",
           from = paste0("p_", from),
           to = paste0("p_", to))
  
  # Artist - group edges
  res <- dbSendQuery(db, paste0("SELECT id_artist, id_group, is_active FROM artist_groups"))
  df_groups <- dbFetch(res) %>%  
    select(from = id_artist,  to = id_group, everything()) %>% 
    mutate(type_edge = "membership",
           from = paste0("p_", from),
           to = paste0("p_", to))
  
  # Artist - alias edges
  res <- dbSendQuery(db, paste0("SELECT id_artist, id_alias FROM artist_aliases"))
  df_aliases <- dbFetch(res)%>% 
    select(from = id_artist,  to = id_alias) %>% 
    mutate(type_edge = "alias",
           from = paste0("p_", from),
           to = paste0("p_", to))
  
  # Collect edges
  df_edges <- bind_rows(df_members, df_groups, df_aliases) %>% unique()
  
  dbWriteTable(db, name_table, df_edges, overwrite = has_table) 
  dbDisconnect(db)
  
  return(df_edges)
}

# Collection items ----
create_collection_item_nodes <- function(file_db){
  
  name_table <- "nodes_collection_items"
  db <- dbConnect(RSQLite::SQLite(), file_db)
  has_table <- dbExistsTable(db, name_table)
  
  res <- dbSendQuery(db, paste0("SELECT * FROM collection_items"))
  df_result <- dbFetch(res)
  
  df_result %<>% 
    select(-id_instance, -starts_with("api_"), -starts_with("qty_")) %>% 
    select(id_node = id_release, name_node = title, url_image = url_cover, everything()) %>% 
    mutate(type_release = "collection_item",
           id_original = id_node,
           id_node = paste0("r_", id_node))
  
  dbWriteTable(db, name_table, df_result, overwrite = has_table) 
  
  dbDisconnect(db)
  
  return(df_result)  
}

create_collection_item_edges <- function(file_db){
  
  name_table <- "edges_collection_items"
  db <- dbConnect(RSQLite::SQLite(), file_db)
  has_table <- dbExistsTable(db, name_table)
  
  res <- dbSendQuery(db, paste0("SELECT id_artist, id_release FROM collection_artists"))
  df_result <- dbFetch(res)
  
  df_result %<>% 
    select(from = id_artist,  to = id_release) %>% 
    mutate(type_edge = "release",
           from = paste0("p_", from),
           to = paste0("r_", to))
  
  dbWriteTable(db, name_table, df_result, overwrite = has_table) 
  
  dbDisconnect(db)
  
  return(df_result)  
}

# Master releases ----
create_master_nodes <- function(file_db){
  
  name_table <- "nodes_master"
  db <- dbConnect(RSQLite::SQLite(), file_db)
  has_table <- dbExistsTable(db, name_table)
  
  res <- dbSendQuery(db, paste0("SELECT artist_releases.*, collection_items.id_instance 
                                 FROM artist_releases
                                 LEFT JOIN collection_items
                                   ON artist_releases.id_release = collection_items.id_master"))
  df_result <- dbFetch(res)
  
  df_result %<>% 
    filter(role %in% c("Main", "Producer", "Co-producer", "Mixed by", "Remix")) %>% 
    mutate(in_collection = !is.na(id_instance),
           year = ifelse(is.na(year), 9999, year)) %>% 
    rename(id_node = id_release) %>% 
    group_by(id_node) %>% 
    summarise(name_node = first(title, order_by = year) ,
              year = min(year, na.rm = TRUE),
              in_collection = max(in_collection),
              url_thumbnail = first(image_thumbnail, order_by = year)) %>% 
    ungroup() %>% 
    mutate(year = ifelse(year == 9999, NA, year),
           type_release = "release",
           id_original = id_node,
           id_node = paste0("r_", id_node))
  
  dbWriteTable(db, name_table, df_result, overwrite = has_table) 
  
  dbDisconnect(db)
  
  return(df_result)  
}

create_master_edges <- function(file_db){
  
  name_table <- "edges_master"
  db <- dbConnect(RSQLite::SQLite(), file_db)
  has_table <- dbExistsTable(db, name_table)
  
  res <- dbSendQuery(db, paste0("SELECT DISTINCT id_artist, id_release, role
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
  
  dbWriteTable(db, name_table, df_result, overwrite = has_table) 
  
  dbDisconnect(db)
  
  return(df_result)  
}
