# Network clustering ----
get_performer_clusters <- function(nw_performer_releases, file_cluster_result, do_cluster_calculation) {
  
  if(do_cluster_calculation){
    
    graph_performer_releases <- graph_from_data_frame(d = nw_performer_releases$df_edges, 
                                                      vertices = nw_performer_releases$df_nodes, 
                                                      directed = FALSE)
    # Removing non-connecting releases
    release_single_performer <- !is.na(V(graph_performer_releases)$type_release) & V(graph_performer_releases)$qty_edges == 1
    graph_performer_releases <- delete_vertices(graph_performer_releases, V(graph_performer_releases)[release_single_performer])
    
    # Do actual clustering 
    clust_performer_releases <- cluster_edge_betweenness(graph_performer_releases, weights = NULL, directed = FALSE)
    write_rds(clust_performer_releases, file_cluster_result)
  } else {
    clust_performer_releases <- read_rds(file_cluster_result)
  }
  
  return(clust_performer_releases)
}

# Get network data frames ----
get_performer_master_network <- function(file_db){
  
  nw_performers <- get_performer_network(file_db) # Get performer network
  
  # Get master release nodes and edges
  db_conn <- dbConnect(RSQLite::SQLite(), file_db)
  df_master_nodes <- dbReadTable(db_conn, 'nodes_master')
  df_master_edges <- dbReadTable(db_conn, 'edges_master')
  dbDisconnect(db_conn)
  
  # Find the number of releases per performer
  df_performer_qty_releases <- df_master_edges %>% 
    rename(id_node = from) %>% 
    group_by(id_node) %>% 
    summarise(qty_masters = n()) %>% 
    ungroup()
  
  nw_performers$df_nodes %<>%
    left_join(df_performer_qty_releases, by = "id_node") %>% 
    mutate(qty_masters = ifelse(is.na(qty_masters), 0, qty_masters))
  
  # Remove performers without own release and just one performer connection
  nw_performers$df_nodes %<>%
    mutate(has_release = qty_masters > 0,
           is_interconnected = qty_performer_connections > 1) %>% 
    filter(has_release | is_interconnected)
    
  # Remove invalid performer connections
  nw_performers$df_edges %<>%
    semi_join(nw_performers$df_nodes, by = c("from" = "id_node" )) %>% 
    semi_join(nw_performers$df_nodes, by = c("to"   = "id_node"))

  # Combine into network list data frames
  lst_network <- list(
    df_nodes = bind_rows(df_master_nodes, nw_performers$df_nodes),
    df_edges = bind_rows(df_master_edges, nw_performers$df_edges)
  )

  
  return(lst_network) 
}

get_performer_network <- function(file_db){

  # Get performer nodes and edges
  db_conn <- dbConnect(RSQLite::SQLite(), file_db)
  df_nodes <- dbReadTable(db_conn, 'nodes_performers')
  df_edges <- dbReadTable(db_conn, 'edges_performers')
  dbDisconnect(db_conn)
  
  # Find the number of connections to other performers
  df_qty_connections <- rbind(
    df_edges %>% select(id_node_from = from, id_node_to = to),
    df_edges %>% select(id_node_from = to,   id_node_to = from) ) %>% 
    unique() %>% 
    select(id_node = id_node_from) %>% 
    group_by(id_node) %>% 
    summarise(qty_performer_connections = n()) %>% 
    ungroup()
  
  df_nodes %<>%
    left_join(df_qty_connections, by = "id_node")
  
  # Combine into network list data frames
  lst_network <- list(
    df_nodes = df_nodes,
    df_edges = df_edges
  )
  
  return(lst_network) 
}

