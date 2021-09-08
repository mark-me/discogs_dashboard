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
df_nodes %<>% left_join(df_clusters, by = "id_node")

rm(df_clusters)

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

rm(df_non_clustered)

# New ----

# Add count of edges per node
graph_performer_releases <- graph_from_data_frame(d = df_edges, vertices = df_nodes, directed = FALSE)
df_qty_edges <- tibble(
  id_node   = V(graph_performer_releases)$name,
  qty_edges = degree(graph_performer_releases)
)
rm(graph_performer_releases)
df_nodes %<>%  left_join(df_qty_edges, by = "id_node")

# TODO: remove
df_nodes %>% group_by(id_node) %>% summarise(qty = n()) %>% ungroup() %>% filter(qty > 1)

# If a cluster contains only releases that connect other clusters duplicate the nodes across clusters ----

# Find nodes of the clusters that only contain releases
df_nodes_release_clusters <- df_nodes %>% 
  group_by(id_cluster) %>% 
  mutate(qty_cluster_nodes    = n(),
         qty_cluster_releases = sum(type_node == "release")) %>% 
  ungroup() %>% 
  filter(qty_cluster_nodes == qty_cluster_releases) %>% 
  filter(qty_edges > 1)

# Find associated edges
df_edges_release_clusters <- df_edges %>% filter(to %in% df_nodes_release_clusters$id_node)

# Make copies of the edges
df_edge_copies <- df_nodes %>%
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
df_nodes %<>% anti_join(df_nodes_release_clusters, by = "id_node")       # Remove old nodes 
df_nodes <- bind_rows(df_nodes, df_nodes_copies)                         # Add copies of nodes
df_edges %<>% anti_join(df_edges_release_clusters, by = c("from", "to")) # Remove old edges
df_edges <- bind_rows(df_edges, df_edge_copies)                          # Add copies of edges

rm(df_nodes_release_clusters, df_nodes_copies, df_edges_release_clusters, df_edge_copies)

# Add _qty_authoritative_ most authoritative performers
df_authoritative <- df_nodes %>% 
  arrange(desc(qty_edges)) %>% 
  group_by(id_cluster) %>% 
  mutate(idx_row_qty_edges = ifelse(type_node == "performer", row_number(), NA)) %>% 
  ungroup() %>% 
  mutate(name_performer = ifelse(idx_row_qty_edges <= 3, name_node, NA)) %>% 
  group_by(id_cluster) %>% 
  mutate(name_performer_clust = paste(na.omit(name_performer), collapse = "\n")) %>% 
  ungroup() %>%
  select(id_node, name_performer_clust)

df_nodes %<>% left_join(df_authoritative, by = "id_node")

rm(df_authoritative)

# Add cluster statistics
df_nodes %<>%
  group_by(id_cluster) %>% 
  mutate(qty_nodes      = n(),
         qty_collection = sum(qty_collection_items, na.rm = TRUE),
         qty_releases   = sum(type_node == "release"),
         qty_performers = sum(type_node == "performer")) %>% 
  ungroup() 

# Contract graph to clusters
graph_clusters <- graph_from_data_frame(d = df_edges, vertices = df_nodes, directed = FALSE)

graph_contracted <- contract(graph_clusters, V(graph_clusters)$id_cluster, vertex.attr.comb = list("first"))
graph_contracted <- simplify(graph_contracted)
graph_contracted <- delete_vertices(graph_contracted, 
                                    V(graph_contracted)[!V(graph_contracted)$is_cluster_visible])


df_nodes <- as_data_frame(graph_contracted, what = "vertices") %>% rename(id = name)
df_edges <- as_data_frame(graph_contracted, what = "edges")

lst_search_result <- list(
  id_step_hierarchy = lst_search_result$id_step_hierarchy,
  df_cluster_ids    = df_nodes %>% select(id, id_cluster, is_cluster_visible),
  nw_cluster        = list(df_nodes = df_nodes, df_edges = df_edges)
)

lst_search_results[[i]] <- get_clustered_network(lst_network, lst_search_results, id_cluster_selected = 3)
lst_search_results[[i]]$nw_cluster$df_nodes %<>% 
  mutate(label = paste0(id_cluster, "-", name_performer))
plot_network(lst_search_results[[i]]$nw_cluster)

i <- 3
lst_search_results[[i]] <- get_clustered_network(lst_network, lst_search_results, id_cluster_selected = 127)
lst_search_results[[i]]$nw_cluster$df_nodes %<>% 
  mutate(label = paste0(id_cluster, "-", name_performer))
plot_network(lst_search_results[[i]]$nw_cluster)  
