library(igraph)

# Get a clustering of performers at a specified point in the clustering hierarchy
get_clusters <- function(res_clustering, id_step = NA, id_cluster_selected = NA){
  
  # If no step is given, the most aggregated level is shown
  if(is.na(id_step)){
    id_step    <- length(res_clustering$merges)/2
    id_cluster <- cut_at(res_clustering, steps = id_step)
    is_visible <- rep(TRUE, length(id_cluster))
  } else {
    # Set the nodes to visible that fall in the selected cluster
    is_visible <- cut_at(res_clustering, steps = id_step) == id_cluster_selected
    
    # Only drill down if there is more than one node in the cluster
    if(sum(is_visible) > 1){
      
      # Drill down the hierarchy until there are at least 15 clusters or the maximum number of nodes is reached
      qty_clusters_min <- ifelse(sum(is_visible) < 15, sum(is_visible), 15)
      qty_clusters <- 0
       while(qty_clusters < qty_clusters_min){
        
        id_step      <- id_step - 100  # Taking the number of steps by 100 to speed up the search process
        id_cluster   <- cut_at(res_clustering, steps = id_step)
        qty_clusters <- length(unique(id_cluster[is_visible]))
      }      
    } else {
      id_cluster   <- cut_at(res_clustering, steps = id_step)
    }
  }
  
  lst_search <- list(
    id_step_hierarchy = id_step,
    df_cluster = tibble(id_nodes   = res_clustering$names,
                        id_cluster = id_cluster,
                        is_visible = is_visible)
  )
  return(lst_search)
}


# Move all releases that do not belong to a cluster to the performer's cluster
move_single_connecting_releases_to_cluster <- function(nw){
  
  df_non_clustered <- nw$df_nodes %>% 
    filter(is.na(id_cluster) | is.na(is_cluster_visible)) %>% 
    select(-id_cluster, -is_cluster_visible) %>% 
    inner_join(select(nw$df_edges, id_node = to, id_artist = from), 
               by = "id_node") %>% 
    inner_join(select(nw$df_nodes, id_artist = id_node, id_cluster, is_cluster_visible),
               by = "id_artist") %>% 
    select(id_node, id_cluster_derived = id_cluster, is_visible_derived = is_cluster_visible)
  
  nw$df_nodes %<>%
    left_join(df_non_clustered, by = "id_node") %>% 
    mutate(id_cluster         = ifelse(is.na(id_cluster), id_cluster_derived, id_cluster),
           is_cluster_visible = ifelse(is.na(is_cluster_visible), is_visible_derived, is_cluster_visible)) %>% 
    select(-id_cluster_derived, -is_visible_derived)
  
  rm(df_non_clustered)
  
  return(nw)
}

# If a cluster contains only releases that connect other clusters duplicate the nodes across clusters
copy_multiple_connecting_releases_to_clusters <- function(nw){
  
  # Find nodes of the clusters that only contain releases
  df_nodes_release_clusters <- nw$df_nodes %>% 
    group_by(id_cluster) %>% 
    mutate(qty_cluster_nodes    = n(),
           qty_cluster_releases = sum(type_node == "release")) %>% 
    ungroup() %>% 
    filter(qty_cluster_nodes == qty_cluster_releases) %>% 
    filter(qty_edges > 1)
  
  # Find associated edges
  df_edges_release_clusters <- nw$df_edges %>% filter(to %in% df_nodes_release_clusters$id_node)
  
  # Make copies of the edges
  df_edge_copies <- nw$df_nodes %>%
    select(id_node, id_cluster) %>% 
    inner_join(df_edges_release_clusters, by = c("id_node" = "from")) %>% 
    rename(from = id_node) %>% 
    unique()
  
  # Make copies of the nodes and provide them with unique ID's
  df_nodes_copies <- df_nodes_release_clusters %>% 
    select(-id_cluster) %>% 
    inner_join(select(df_edge_copies, to, id_cluster) %>% unique(),
               by = c("id_node" = "to")) %>% 
    mutate(id_node = paste0(id_node, "_", id_cluster))
  
  # Adjust edge copies with new node ID's
  df_edge_copies %<>%
    mutate(to = paste0(to, "_", id_cluster)) %>% 
    select(-id_cluster)
  
  # Rebuild network
  nw$df_nodes %<>% anti_join(df_nodes_release_clusters, by = "id_node")       # Remove old nodes 
  nw$df_nodes <- bind_rows(nw$df_nodes, df_nodes_copies)                      # Add copies of nodes
  nw$df_edges %<>% anti_join(df_edges_release_clusters, by = c("from", "to")) # Remove old edges
  nw$df_edges <- bind_rows(nw$df_edges, df_edge_copies)                       # Add copies of edges
  
  rm(df_nodes_release_clusters, df_nodes_copies, df_edges_release_clusters, df_edge_copies)  
  
  return(nw)
}

# Add count of edges per node
add_edge_count <- function(nw){
  
  graph_performer_releases <- graph_from_data_frame(d = nw$df_edges, vertices = nw$df_nodes, directed = FALSE)
  df_qty_edges <- tibble(
    id_node   = V(graph_performer_releases)$name,
    qty_edges = degree(graph_performer_releases)
  )
  rm(graph_performer_releases)
  nw$df_nodes %<>%  left_join(df_qty_edges, by = "id_node")
  
  return(nw)
}

# Add _qty_authoritative_ most authoritative performers as a cluster description
add_most_authoritative_names <- function(nw, qty_authoritative = 3){
  
  df_authoritative <- nw$df_nodes %>% 
    arrange(desc(qty_collection_items),desc(qty_edges)) %>% 
    group_by(id_cluster) %>% 
    mutate(idx_row_qty_edges = ifelse(type_node == "performer", row_number(), NA)) %>% 
    ungroup() %>% 
    mutate(name_performer = ifelse(idx_row_qty_edges <= qty_authoritative, name_node, NA)) %>% 
    group_by(id_cluster) %>% 
    mutate(name_authoritative = paste(na.omit(name_performer), collapse = "\n")) %>% 
    ungroup() %>%
    select(id_node, name_authoritative)
  
  nw$df_nodes %<>% left_join(df_authoritative, by = "id_node")
  
  rm(df_authoritative)
  
  return(nw)  
}

# Only add performer images
add_performer_image <- function(nw){
  
  df_images <- nw$df_nodes %>%
    filter(type_node == "performer") %>% 
    group_by(id_cluster) %>% 
    summarise(url_thumbnail_performer = first(url_thumbnail)) %>% 
    ungroup() 
  
  nw$df_nodes %<>%
    left_join(df_images, by = "id_cluster") %>% 
    mutate(url_thumbnail = url_thumbnail_performer) %>% 
    select(-url_thumbnail_performer)
  
  return(nw)
}
# Add cluster statistics
add_cluster_statistics <- function(nw){
  
  nw$df_nodes %<>%
    group_by(id_cluster) %>% 
    mutate(qty_nodes      = n(),
           qty_collection = sum(qty_collection_items, na.rm = TRUE),
           qty_releases   = sum(type_node == "release"),
           qty_performers = sum(type_node == "performer")) %>% 
    ungroup() 
  
  return(nw)
}

aggregate_network <- function(nw){
  
  graph_network <- graph_from_data_frame(d = nw$df_edges, vertices = nw$df_nodes, directed = FALSE)
  
  # Create a aggregated graph
  graph_contracted <- contract(graph_network, V(graph_network)$id_cluster, vertex.attr.comb = list("first"))
  graph_contracted <- simplify(graph_contracted)
  graph_contracted <- delete_vertices(graph_contracted, 
                                      V(graph_contracted)[!V(graph_contracted)$is_cluster_visible])
  
  nw_contracted <- list(
    df_nodes = as_data_frame(graph_contracted, what = "vertices") %>% rename(id = name), 
    df_edges = as_data_frame(graph_contracted, what = "edges")
    )
  return(nw_contracted)
}

# Get a clustering of performers at a specified point in the clustering hierarchy, returning an aggregated network
get_clustered_network <- function(lst_network, lst_search_results = NA, id_cluster_selected = NA){
  
  nw <- lst_network$nw_performer_releases
  
  if(is.na(lst_search_results)){
    # Get least number of clusters if no previous search results are provided
    lst_search_result <- get_clusters(lst_network$res_clustering)  
  } else {
    # Get clustering based on the previous aggregation level and a chosen node within that aggregation
    lst_search_result <- get_clusters(res_clustering = lst_network$res_clustering,
                                      id_step = lst_search_results[[length(lst_search_results)]]$id_step, 
                                      id_cluster_selected = id_cluster_selected) 
  }
  
  # Add cluster id's to the nodes
  df_clusters <- tibble(id_node            = lst_search_result$df_cluster$id_nodes,
                        id_cluster         = lst_search_result$df_cluster$id_cluster,
                        is_cluster_visible = lst_search_result$df_cluster$is_visible )
  nw$df_nodes %<>% left_join(df_clusters, by = "id_node") # Add cluster ID's to nodes
  
  # Set single connecting releases cluster to the performer's cluster ID 
  nw <-move_single_connecting_releases_to_cluster(nw)
  nw <- add_edge_count(nw)  # Add count of edges per node
  # If a cluster contains only releases that connect other clusters duplicate the nodes across clusters
  nw <- copy_multiple_connecting_releases_to_clusters(nw)
  nw <- add_performer_image(nw)
  nw <- add_most_authoritative_names(nw, qty_authoritative = 3) # Add _qty_authoritative_ most authoritative performers as a cluster description
  nw <- add_cluster_statistics(nw)         # Add cluster statistics
  nw_contracted <- aggregate_network(nw)   # Contract network to clusters
  
  # Temp fix: replacing node id's with cluster id's
  nw_contracted$df_edges %<>% 
    left_join(select(nw_contracted$df_nodes, id, id_cluster),
              by = c("from" = "id")) %>% 
    mutate(from = id_cluster) %>% 
    select(-id_cluster) %>% 
    left_join(select(nw_contracted$df_nodes, id, id_cluster),
              by = c("to" = "id")) %>% 
    mutate(to = id_cluster) %>% 
    select(-id_cluster) 
  
  nw_contracted$df_nodes %<>%
    mutate(id = id_cluster) %>% 
    mutate(label = name_authoritative)
  
  # Compile return object
  lst_search_result <- list(
    id_step_hierarchy = lst_search_result$id_step_hierarchy,
    df_cluster_ids    = nw$df_nodes %>% select(id_node, id_cluster, is_cluster_visible),
    nw_cluster        = nw_contracted
  )
  return(lst_search_result)
}


plot_network <- function(network){
  
  network$df_nodes %<>%
    mutate(image = ifelse(qty_performers == 1, url_thumbnail, NA),
           shape = ifelse(qty_performers == 1, "image", "icon"),
           icon.code = "f0c0")
  
  visNetwork(network$df_nodes, network$df_edges
             , background = "black") %>% 
    visNodes(
      color = list(background = "lightblue",
                   border = "grey",
                   highlight = "yellow"),
      font = list(color = "white",
                  strokeWidth = 3,
                  strokeColor = alpha("black", alpha = 0.7)),
      shadow = list(enabled = TRUE, size = 10))  %>%
    visOptions(highlightNearest = TRUE) %>%
    visPhysics(maxVelocity = 10) %>%
    addFontAwesome()
}

get_cluster_performers <- function(df_network_nodes, df_cluster_ids){
  
  df_performers <- df_network_nodes %>% 
    filter(type_node == "performer") %>% 
    inner_join(df_cluster_ids, by = "id_node") %>% 
    filter(is_cluster_visible) %>% 
    select(-year, -in_collection, -is_cluster_visible)
  
  return(df_performers)
}

get_cluster_releases <- function(df_network_nodes, df_cluster_ids){
  
  df_releases <- df_network_nodes %>%   
    filter(type_node == "release") %>% 
    select(-starts_with("qty_"), -name_artist_real, -profile, -url_artist_discogs,
           -api_releases, -url_image, -type_performer, -is_active, -role_primary, -has_release) %>% 
    inner_join(df_cluster_ids, by = "id_node") %>% 
    filter(is_cluster_visible) %>% 
    select(-is_cluster_visible)
    
  return(df_releases)
}


example_cluster_navigation <- function(){
 
  file_db <- '~/Development/Datasets/discogs_dashboard/discogs.sqlite'
  file_cluster_result <- 'cluster_edge_betweenness.rds'
  
  # Create an object containing the network and its clustering result
  lst_network <- list(
    nw_performer_releases = get_performer_master_network(file_db),
    res_clustering        = get_performer_clusters(nw_performer_releases, 
                                                   file_cluster_result, 
                                                   do_cluster_calculation = FALSE)
  )
  
  # A list that will contain search results
  lst_search_results <- list()
  
  i <- 1
  lst_search_results[[i]] <- get_clustered_network(lst_network, lst_search_results = NA, id_cluster_selected = NA)
  lst_search_results[[i]]$nw_cluster$df_nodes %<>% 
    mutate(label = paste0(id_cluster, "-", name_performer))
  plot_network(lst_search_results[[i]]$nw_cluster)
  
  i <- 2
  lst_search_results[[i]] <- get_clustered_network(lst_network, lst_search_results, id_cluster_selected = 3)
  lst_search_results[[i]]$nw_cluster$df_nodes %<>% 
    mutate(label = paste0(id_cluster, "-", name_performer))
  plot_network(lst_search_results[[i]]$nw_cluster)
  
  i <- 3
  lst_search_results[[i]] <- get_clustered_network(lst_network, lst_search_results, id_cluster_selected = 127)
  lst_search_results[[i]]$nw_cluster$df_nodes %<>% 
    mutate(label = paste0(id_cluster, "-", name_performer))
  plot_network(lst_search_results[[i]]$nw_cluster)  
}
