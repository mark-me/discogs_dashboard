library(tidyverse)
library(magrittr)
library(RSQLite)
library(yaml)
library(scales)
library(ggimage)
library(visNetwork)
source("discogs_dashboard/get_network_data_functions.R")
source("discogs_dashboard/cluster_navigation.R")

config <- read_yaml("config.yml")
file_db <- paste0(config$db_location, "/discogs.sqlite")

lst_network <- list(
  res_clustering = read_rds(config$file_cluster_results),
  nw_performer_releases = get_performer_master_network(file_db)
)

lst_search_results <- list()

i <- 1
lst_search_results[[i]] <- get_clustered_network(lst_network, lst_search_results = NA, id_cluster_selected = NA)
lst_search_results[[i]]$nw_cluster$df_nodes %<>% 
  mutate(label = paste0(id_cluster, "-", name_performer))
plot_network(lst_search_results[[i]]$nw_cluster)

i <- 2
id_cluster_selected <- 3


lst_search_result <- get_clusters(res_clustering = lst_network$res_clustering,
                                  id_step = lst_search_results[[length(lst_search_results)]]$id_step, 
                                  id_cluster_selected = id_cluster_selected) 



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
graph_clusters <- graph_performer_releases

df_agg_node_attr <- aggregate_node_attributes(graph_clusters, qty_authoritative = 3)
V(graph_clusters)[df_agg_node_attr$name]$name_performer <- df_agg_node_attr$name_performer_clust
V(graph_clusters)[df_agg_node_attr$name]$qty_nodes      <- df_agg_node_attr$qty_nodes
V(graph_clusters)[df_agg_node_attr$name]$qty_edges      <- df_agg_node_attr$qty_edges
V(graph_clusters)[df_agg_node_attr$name]$qty_collection <- df_agg_node_attr$qty_collection
V(graph_clusters)[df_agg_node_attr$name]$qty_releases   <- df_agg_node_attr$qty_releases
V(graph_clusters)[df_agg_node_attr$name]$qty_performers <- df_agg_node_attr$qty_performers

# If a cluster contains only releases ----
has_only_releases <- V(graph_clusters)$qty_nodes == V(graph_clusters)$qty_releases

# and is connected to one other cluster, merge it into that cluster
has_multiple_connections <- V(graph_clusters)$qty_edges > 1

# and is connected to multiple nodes, put copies of it's nodes in both clusters and remove the node and create links between the adjoining nodes





vertex_attr_names(graph_clusters)






lst_search_results[[i]] <- get_clustered_network(lst_network, lst_search_results, id_cluster_selected = 3)
lst_search_results[[i]]$nw_cluster$df_nodes %<>% 
  mutate(label = paste0(id_cluster, "-", name_performer))
plot_network(lst_search_results[[i]]$nw_cluster)

i <- 3
lst_search_results[[i]] <- get_clustered_network(lst_network, lst_search_results, id_cluster_selected = 127)
lst_search_results[[i]]$nw_cluster$df_nodes %<>% 
  mutate(label = paste0(id_cluster, "-", name_performer))
plot_network(lst_search_results[[i]]$nw_cluster)  
