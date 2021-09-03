library(igraph)

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
      
      # Drill down the hierarchy until there are at least 6 clusters or the maximum number of nodes is reached
      qty_clusters_min <- ifelse(sum(is_visible) < 6, sum(is_visible), 6)
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

aggregate_node_attributes <- function(graph_clusters, qty_authoritative){

  # Find most authoritative performers based on number of connections
  qty_node_edges <- degree(graph_clusters)
  V(graph_clusters)$qty_edges <- qty_node_edges
    
  df_nodes <- as_data_frame(graph_clusters, what = "vertices")

  # Find _qty_authoritative_ most authoritative performers
  df_authoritative <- df_nodes %>% 
    arrange(desc(qty_edges)) %>% 
    group_by(id_cluster) %>% 
    mutate(idx_row_qty_edges = ifelse(type_node == "performer", row_number(), NA)) %>% 
    ungroup() %>% 
    mutate(name_performer = ifelse(idx_row_qty_edges <= qty_authoritative, name_node, NA)) %>% 
    group_by(id_cluster) %>% 
    mutate(name_performer_clust = paste(na.omit(name_performer), collapse = "\n")) %>% 
    ungroup() %>%
    select(name, name_performer_clust)
  
  df_nodes %<>%
    left_join(df_authoritative, by = "name") %>% 
    group_by(id_cluster) %>% 
    mutate(qty_nodes_clust      = n(),
           qty_masters_clust   = sum(qty_masters, na.rm = TRUE),
           qty_collection_clust = sum(qty_collection_items, na.rm = TRUE)) %>% 
    ungroup() %>% 
    select(name, id_cluster, name_performer_clust, qty_nodes_clust, qty_masters_clust, qty_collection_clust)

  # Count performers and releases per node
  df_nodes %<>%
    group_by(id_cluster) %>% 
    mutate() %>% 
    ungroup()
  
  return(df_nodes)
}

aggregate_network <- function(graph_clusters){
  
  # Aggregate release network to the clusters 
  df_agg_node_attr <- aggregate_node_attributes(graph_clusters, qty_authoritative = 3)
  V(graph_clusters)[df_agg_node_attr$name]$name_performer <- df_agg_node_attr$name_performer_clust
  V(graph_clusters)[df_agg_node_attr$name]$qty_nodes      <- df_agg_node_attr$qty_nodes_clust
  V(graph_clusters)[df_agg_node_attr$name]$qty_collection <- df_agg_node_attr$qty_collection_clust
  V(graph_clusters)[df_agg_node_attr$name]$qty_releases   <- df_agg_node_attr$qty_masters_clust
  
  # Create a aggregated graph
  graph_contracted <- contract(graph_clusters, V(graph_clusters)$id_cluster, vertex.attr.comb = list("first"))
  graph_contracted <- simplify(graph_contracted)
  graph_contracted <- delete_vertices(graph_contracted, 
                                      V(graph_contracted)[!V(graph_contracted)$is_cluster_visible])
  
  return(graph_contracted)
}

get_clustered_network <- function(lst_network, lst_search_results = NA, id_cluster_selected = NA){
  
  # Get least number of clusters if no previous search results are provided
  if(is.na(lst_search_results)){
    lst_search_result <- get_clusters(lst_network$res_clustering)  
  } else {
    lst_search_result <- get_clusters(res_clustering = lst_network$res_clustering,
                                      id_step = lst_search_results[[length(lst_search_results)]]$id_step, 
                                      id_cluster_selected = id_cluster_selected) 
  }
  
  df_clusters <- tibble(
    id_node            = lst_search_result$df_cluster$id_nodes,
    id_cluster         = lst_search_result$df_cluster$id_cluster,
    is_cluster_visible = lst_search_result$df_cluster$is_visible
  )
  
  # Add cluster ID's to nodes
  df_nodes <- lst_network$nw_performer_releases$df_nodes
  df_edges <- lst_network$nw_performer_releases$df_edges
  df_nodes %<>%
    left_join(df_clusters, by = "id_node")
  
  # Non-connecting release nodes won't have a cluster ID. They will get the performer's cluster ID 
  df_non_clustered <- df_nodes %>% 
    filter(is.na(id_cluster) | is.na(is_cluster_visible)) %>% 
    select(-id_cluster, -is_cluster_visible) %>% 
    inner_join(select(df_edges, id_node = to, id_artist = from), 
               by = "id_node") %>% 
    inner_join(select(df_nodes, id_artist = id_node, id_cluster, is_cluster_visible),
               by = "id_artist") %>% 
    select(id_node, id_cluster_derived = id_cluster, is_visible_derived = is_cluster_visible)
  
  df_nodes %<>%
    left_join(df_non_clustered, by = "id_node") %>% 
    mutate(id_cluster         = ifelse(is.na(id_cluster), id_cluster_derived, id_cluster),
           is_cluster_visible = ifelse(is.na(is_cluster_visible), is_visible_derived, is_cluster_visible)) %>% 
    select(-id_cluster_derived, -is_visible_derived)
  
  # Contract network to clusters
  graph_performer_releases <- graph_from_data_frame(d = df_edges, vertices = df_nodes, directed = FALSE)
  graph_clusters <- aggregate_network(graph_performer_releases)
  
  df_nodes <- as_data_frame(graph_clusters, what = "vertices") %>% rename(id = name)
  df_edges <- as_data_frame(graph_clusters, what = "edges")
  
  lst_search_result <- list(
    id_step_hierarchy = lst_search_result$id_step_hierarchy,
    nw_cluster        = list(df_nodes = df_nodes, df_edges = df_edges)
  )
  
  return(lst_search_result)
}

plot_network <- function(network){
  
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
    visPhysics(maxVelocity = 10)
}

file_db <- '~/Development/Datasets/discogs_dashboard/discogs.sqlite'
file_cluster_result <- 'cluster_edge_betweenness.rds'

# Create an object containing the network and its clustering result
lst_network <- list(
  nw_performer_releases = get_performer_master_network(file_db),
  res_clustering        = get_performer_clusters(nw_performer_releases, 
                                                 file_cluster_result, 
                                                 do_cluster_calculation = FALSE)
)

example_cluster_navigation <- function(){
  
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
