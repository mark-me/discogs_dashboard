options(dplyr.summarise.inform = FALSE)

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

cluster_to_network <- function(graph_clusters){
  
  df_nodes <- as_data_frame(graph_clusters, what = "vertices") %>% 
    rename(id = name) %>% 
    mutate(label = paste0(cluster, " - ", qty_nodes, "\n", name_performer))
  
  df_edges <- as_data_frame(graph_clusters, what = "edges")
  
  return(list(df_nodes = df_nodes,
              df_edges = df_edges))
}

get_cluster <- function(graph_rel, res_clust, search_item, search_item_previous = NA){

  # Iterate through network from top down   
  if(is.na(search_item_previous)){
    
    qty_steps <- length(res_clust$merges)/2
    id_communities <- cut_at(res_clust, steps = qty_steps)
    is_cluster_selected <- rep(TRUE, length(id_communities))

  } else {
    
    qty_steps <- search_item_previous$qty_steps - 1
    is_cluster_selected <- search_item_previous$id_communities == search_item$id_cluster_selected
    
    qty_clusters_min <- 6
    qty_clusters <- 0
    while(qty_clusters < qty_clusters_min){
      
      id_communities <- cut_at(res_clust, steps = qty_steps)
      qty_steps <- qty_steps - 1
      qty_clusters <- length(unique(id_communities[is_cluster_selected]))
    }

  }
  
  V(graph_rel)$cluster             <- id_communities
  V(graph_rel)$is_cluster_selected <- is_cluster_selected
  graph_aggr <- aggregate_network(graph_rel)
  
  search_item = append(search_item, 
                       list(qty_steps = qty_steps,
                            id_communities = id_communities,
                            is_cluster_selected = is_cluster_selected,
                            graph = graph_aggr))  
  return(search_item)
}


aggregate_network <- function(graph_clusters){
  
  # Aggregate release network to the clusters 
  df_agg_node_attr <- aggregate_node_attributes(graph_clusters, qty_authoritative = 3)
  V(graph_clusters)[df_agg_node_attr$name]$name_performer <- df_agg_node_attr$name_performer_clust
  V(graph_clusters)[df_agg_node_attr$name]$qty_nodes      <- df_agg_node_attr$qty_nodes_clust
  V(graph_clusters)[df_agg_node_attr$name]$qty_collection <- df_agg_node_attr$qty_collection_clust
  V(graph_clusters)[df_agg_node_attr$name]$qty_releases   <- df_agg_node_attr$qty_releases_clust
  
  # Create a aggregated graph
  graph_contracted <- contract(graph_clusters, V(graph_clusters)$cluster, vertex.attr.comb = list("first"))
  graph_contracted <- simplify(graph_contracted)
  graph_contracted <- delete_vertices(graph_contracted, 
                                      V(graph_contracted)[!V(graph_contracted)$is_cluster_selected])
  
  return(graph_contracted)
}

aggregate_node_attributes <- function(graph_clusters, qty_authoritative){
  
  df_nodes <- as_data_frame(graph_clusters, what = "vertices")
  
  # Find _qty_authoritative_ most authoritative performers
  df_authoritative <- df_nodes %>% 
    arrange(desc(qty_edges)) %>% 
    group_by(cluster) %>% 
    mutate(idx_row_qty_edges = row_number()) %>% 
    ungroup() %>% 
    mutate(name_performer = ifelse(idx_row_qty_edges > 3, "", name_node)) %>% 
    group_by(cluster) %>% 
    mutate(name_performer_clust = paste(name_performer, collapse = "\n")) %>% 
    ungroup() %>% 
    mutate(name_performer_clust = str_trim(name_performer_clust)) %>% 
    select(name, name_performer_clust)
  
  df_nodes %<>%
    left_join(df_authoritative, by = "name") %>% 
    group_by(cluster) %>% 
    mutate(qty_nodes_clust      = n(),
           qty_releases_clust   = sum(qty_releases, na.rm = TRUE),
           qty_collection_clust = sum(qty_collection_items, na.rm = TRUE)) %>% 
    ungroup() %>% 
    select(name, cluster, name_performer_clust, qty_nodes_clust, qty_releases_clust, qty_collection_clust)
  
  return(df_nodes)
}

plot_clusters <- function(graph_clust){
  V(graph_clust)$label <- paste0(V(graph_clust)$cluster, " - ", 
                                V(graph_clust)$qty_nodes, "\n",
                                V(graph_clust)$name_performer
  )
  
  V(graph_clust)$color <- ifelse(!is.na(V(graph_clust)$type_release), "yellow", "orange")
  
  plot(graph_clust)
}

test_navigation <- function(){
  # Create test search path
  lst_search_item <- list()
  i <- 1
  lst_search_item[[i]] <- list()
  lst_search_item[[i]] <- get_cluster(graph_rel   = graph_releases,
                                      res_clust   = clust_releases, 
                                      search_item = lst_search_item[[i]])
  
  plot_clusters(lst_search_item[[i]]$graph)
  
  i <- i + 1
  lst_search_item[[i]] <- list(id_cluster_selected = 3)
  lst_search_item[[i]] <- get_cluster(graph_rel   = graph_releases,
                                      res_clust   = clust_releases, 
                                      search_item = lst_search_item[[i]],
                                      search_item_previous = lst_search_item[[i-1]])
  
  plot_clusters(lst_search_item[[2]]$graph)
  
  i <- i + 1
  lst_search_item[[i]] <- list(id_cluster_selected = 225)
  lst_search_item[[i]] <- get_cluster(graph_rel   = graph_releases,
                                      res_clust   = clust_releases, 
                                      search_item = lst_search_item[[i]],
                                      search_item_previous = lst_search_item[[i-1]])
  
  plot_clusters(lst_search_item[[i]]$graph)
  
  i <- i + 1
  lst_search_item[[i]] <- list(id_cluster_selected = 771)
  lst_search_item[[i]] <- get_cluster(graph_rel   = graph_releases,
                                      res_clust   = clust_releases, 
                                      search_item = lst_search_item[[i]],
                                      search_item_previous = lst_search_item[[i-1]])
  
  plot_clusters(lst_search_item[[i]]$graph)
  
  i <- i + 1
  lst_search_item[[i]] <- list(id_cluster_selected = 371)
  lst_search_item[[i]] <- get_cluster(graph_rel   = graph_releases,
                                      res_clust   = clust_releases, 
                                      search_item = lst_search_item[[i]],
                                      search_item_previous = lst_search_item[[i-1]])
  
  plot_clusters(lst_search_item[[i]]$graph)
}
