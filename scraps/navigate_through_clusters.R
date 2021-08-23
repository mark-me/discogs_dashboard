library(httr)
library(rjson)
library(tidyverse)
library(magrittr)
library(yaml)
library(RSQLite)
library(igraph)
library(visNetwork)

source("data-prep/get_network_data_functions.R")
source("data-prep/calculate_artist_similarity.R")
config <- read_yaml("config.yml")

graph_releases <- get_artist_clusters(FALSE)
clust_releases <- read_rds("cluster_edge_betweenness.rds")

get_cluster <- function(graph_releases, clust_releases, name_node_selected = NA){
  
  # Cluster until the number of clusters in the sub graph
  qty_clusters_min <- 6
  qty_clusters <- 0
  no_communities <- qty_clusters_min
  while(qty_clusters < qty_clusters_min){
    
    no_communities <- no_communities + 1
    id_communities <- cut_at(clust_releases, no = no_communities)
    V(graph_releases)$cluster <- id_communities
    
    if(!is.na(name_node_selected)){
      graph_clusters <- make_ego_graph(graph_releases, 
                                       order = length(V(graph_releases)),
                                       nodes = V(graph_releases)[name_node_selected])[[1]]
    } else {
      graph_clusters <- graph_releases 
    }
    
    qty_clusters <- length(unique(V(graph_clusters)$cluster))
  }
  
  # Aggregate node data
  df_agg_node_attr <- aggregate_node_attributes(graph_clusters, qty_authoritative = 3)
  V(graph_clusters)[df_agg_node_attr$name]$name_performer <- df_agg_node_attr$name_performer_clust
  V(graph_clusters)[df_agg_node_attr$name]$qty_nodes      <- df_agg_node_attr$qty_nodes_clust
  V(graph_clusters)[df_agg_node_attr$name]$qty_collection <- df_agg_node_attr$qty_collection_clust
  V(graph_clusters)[df_agg_node_attr$name]$qty_releases   <- df_agg_node_attr$qty_releases_clust
  
  # Create a aggregated graph
  graph_contracted <- contract(graph_clusters, V(graph_clusters)$cluster,
                               vertex.attr.comb = list("first"))
  graph_contracted <- simplify(graph_contracted)
  
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

# # Make a subcluster of the chosen cluster one level lower than previous
# graph_selected <- get_cluster(graph_releases, clust_releases)
# V(graph_selected)$label <- paste0(V(graph_selected)$cluster, " - ", 
#                                   V(graph_selected)$qty_nodes, "\n",
#                                   V(graph_selected)$name_performer
#                                   )
# plot(graph_selected)
# 
# name_node_selected <- "p_1003228"
# V(graph_selected)[name_node_selected]$name_performer
# V(graph_selected)[name_node_selected]$cluster
# graph_selected <- get_cluster(graph_releases, clust_releases, name_node_selected)
# V(graph_selected)$label <- paste0(V(graph_selected)$cluster, " - ", 
#                                   V(graph_selected)$qty_nodes, "\n",
#                                   V(graph_selected)$name_performer
# )
# plot(graph_selected)
# 
# name_node_selected <- "p_2184902"
# V(graph_selected)[name_node_selected]$name_performer
# V(graph_selected)$label <- paste0(V(graph_selected)$name, " - ", 
#                                   V(graph_selected)$qty_nodes, "\n",
#                                   V(graph_selected)$name_performer
# )
# V(graph_selected)[name_node_selected]$name_performer
# plot(graph_selected)

# Gets stuck at a level

# Second go
get_next_iter <- function(res_clust, search_item, search_item_previous = NA){
  
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
  
  search_item = append(search_item, 
                       list(qty_steps = qty_steps,
                            id_communities = id_communities,
                            is_cluster_selected = is_cluster_selected))  
  return(search_item)
}

aggregate_network <- function(graph_clusters){
  
  df_agg_node_attr <- aggregate_node_attributes(graph_clusters, qty_authoritative = 3)
  V(graph_clusters)[df_agg_node_attr$name]$name_performer <- df_agg_node_attr$name_performer_clust
  V(graph_clusters)[df_agg_node_attr$name]$qty_nodes      <- df_agg_node_attr$qty_nodes_clust
  V(graph_clusters)[df_agg_node_attr$name]$qty_collection <- df_agg_node_attr$qty_collection_clust
  V(graph_clusters)[df_agg_node_attr$name]$qty_releases   <- df_agg_node_attr$qty_releases_clust
  
  # Create a aggregated graph
  graph_contracted <- contract(graph_clusters, V(graph_clusters)$cluster,
                               vertex.attr.comb = list("first"))
  graph_contracted <- simplify(graph_contracted)
  
  graph_contracted <- delete_vertices(graph_contracted, 
                                      V(graph_contracted)[!V(graph_contracted)$is_selected])
  
  return(graph_contracted)
}

lst_search_item <- list()
lst_search_item[[1]] <- list()

lst_search_item[[1]] <- get_next_iter(clust_releases, 
                                      search_item = lst_search_item[[1]])

search_item <- lst_search_item[[1]]
graph_clusters <- graph_releases 
V(graph_clusters)$cluster <- search_item$id_communities
V(graph_clusters)$is_selected <- search_item$is_cluster_selected
#graph_clusters <- delete_vertices(graph_clusters, V(graph_clusters)[!search_item$is_cluster_selected])
graph_aggr <- aggregate_network(graph_clusters)
V(graph_aggr)$label <- paste0(V(graph_aggr)$cluster, " - ", 
                              V(graph_aggr)$qty_nodes, "\n",
                              V(graph_aggr)$name_performer
                              )
plot(graph_aggr)

lst_search_item[[2]] <- list(id_cluster_selected = 3)
lst_search_item[[2]] <- get_next_iter(clust_releases, 
                                      search_item = lst_search_item[[2]],
                                      search_item_previous = lst_search_item[[1]])

search_item <- lst_search_item[[2]]
graph_clusters <- graph_releases 
V(graph_clusters)$cluster     <- search_item$id_communities
V(graph_clusters)$is_selected <- search_item$is_cluster_selected
graph_aggr <- aggregate_network(graph_clusters)
V(graph_aggr)$label <- paste0(V(graph_aggr)$cluster, " - ", 
                              V(graph_aggr)$qty_nodes, "\n",
                              V(graph_aggr)$name_performer
)
plot(graph_aggr)


lst_search_item[[3]] <- list(id_cluster_selected = 225)
lst_search_item[[3]] <- get_next_iter(clust_releases, 
                                      search_item = lst_search_item[[3]],
                                      search_item_previous = lst_search_item[[2]])

search_item <- lst_search_item[[3]]
graph_clusters <- graph_releases 
V(graph_clusters)$cluster <- search_item$id_communities
V(graph_clusters)$is_selected <- search_item$is_cluster_selected
graph_aggr <- aggregate_network(graph_clusters)
V(graph_aggr)$label <- paste0(V(graph_aggr)$cluster, " - ", 
                              V(graph_aggr)$qty_nodes, "\n",
                              V(graph_aggr)$name_performer
)
plot(graph_aggr)


with(search_item, table(id_communities[is_cluster_selected]))
table(V(graph_aggr)$cluster, useNA = "ifany")

