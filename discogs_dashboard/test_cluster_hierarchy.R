library(tidyverse)
library(magrittr)
library(RSQLite)
library(yaml)
library(scales)
library(ggimage)
library(igraph)
library(visNetwork)
source("discogs_dashboard/get_network_data_functions.R")
source("discogs_dashboard/cluster_navigation.R")

config <- read_yaml("config.yml")
file_db <- paste0(config$db_location, "/discogs.sqlite")

lst_network <- list(
  res_clustering = read_rds(config$file_cluster_results),
  nw_performer_releases = get_performer_master_network(file_db)
)

res_clustering <- lst_network$res_clustering
nw <- lst_network$nw_performer_releases

# Make graph of network  
graph_network <- graph_from_data_frame(d = nw$df_edges, vertices = nw$df_nodes, directed = FALSE)

id_cluster <- cut_at(res_clustering, steps = 0)
df_leaves <- tibble(
  id_node = res_clustering$names,
  from = id_cluster
) %>% 
  left_join(nw$df_nodes, by = "id_node")

# Add original node names to merges
mat_merges <- res_clustering$merges
colnames(mat_merges) <- c('to', 'from')
df_merges <- as_tibble(mat_merges) 
df_merges %<>% 
  left_join(df_leaves, by = 'from') %>% 
  mutate(from = ifelse(!is.na(id_node), id_node, from)) %>% 
  select(from, to, 
         name_from = name_node, type_from = type_node, is_interconnected_from = is_interconnected) %>% 
  left_join(df_leaves, by = c('to' = 'from')  ) %>% 
  mutate(to = ifelse(!is.na(id_node), id_node, to)) %>% 
  select(from, to, name_from, type_from, is_interconnected_from,
         name_to = name_node, type_to = type_node, is_interconnected_to = is_interconnected)

# Pruning release only vertices
graph_cluster_hierarchy <- graph_from_data_frame(df_merges, directed = TRUE)




df_cluster$id_cluster


nw <- lst_network$nw_performer_releases
df_nodes_artists <- nw$df_nodes %>% filter(type_node == "performer")
V(graph_hierarchy)[df_nodes_artists$id_node]$label <- df_nodes_artists$name_node
V(graph_hierarchy)[is.na(V(graph_hierarchy)$label)]$label <- V(graph_hierarchy)$name
V(graph_hierarchy)[df_nodes_artists$id_node]$is_performer <- TRUE
V(graph_hierarchy)[is.na(V(graph_hierarchy)$is_performer)]$is_performer <- FALSE

table(V(graph_hierarchy)$is_performer, useNA = "ifany")

order_no <- 0
qty_artists <- 0
while(qty_artists < 15){
  order_no        <- order_no + 1
  print(order_no)
  graph_performer <- make_ego_graph(graph_hierarchy, 
                                    order = order_no, 
                                    nodes = V(graph_hierarchy)[id_node_perfomer],
                                    mode = "all")[[1]]
  qty_artists     <- sum(V(graph_performer)$is_performer)
}

plot(graph_performer, layout_as_tree(graph_performer))

# Try to build clustering tree with merges: FAILED ----
mat_merges <- res_clustering$merges
colnames(mat_merges) <- c('a', 'b')
df_merges <- as_tibble(mat_merges) %>% 
  mutate(id_step = row_number())

graph_merges <- graph_from_data_frame(df_merges, directed = TRUE)

graph_artist_merges <- make_ego_graph(graph_merges, nodes = V(graph_merges)[id_cluster_performer], order = 9999)[[1]]

plot(graph_artist_merges, layout_as_tree(graph_artist_merges))
