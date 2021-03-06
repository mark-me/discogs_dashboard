library(httr)		# Library for scraping web data
library(rjson)
library(tidyverse)
library(magrittr)
library(yaml)
library(RSQLite)
library(igraph)
library(dendextend)
library(ape)

config <- read_yaml("config.yml")
source("data-prep/get_network_data_functions.R")
source("data-prep/calculate_artist_similarity.R")

graph_releases <- get_graph_releases()

# What are the vertex attributes again?
list.vertex.attributes(graph_releases)

# Calculate the representative artists
qty_node_edges <- degree(graph_releases)
V(graph_releases)$qty_edges <- qty_node_edges

# See the most authoritative performers
hase_a_lot_of_edges <- V(graph_releases)$qty_edges > 300
V(graph_releases)[hase_a_lot_of_edges]$name_node

# Removing non-connecting releases
release_single_performer <- !is.na(V(graph_releases)$type_release) & V(graph_releases)$qty_edges == 1
graph_connecting <- delete_vertices(graph_releases, V(graph_releases)[release_single_performer])

# Create communities and review the most authoritative performers of each community
#clust_releases <- cluster_fast_greedy(graph_releases)
#clust <- cluster_edge_betweenness(graph_releases, weights = NULL, directed = FALSE)
#write_rds(clust, "cluster_edge_betweenness.rds")
clust_releases <- read_rds("cluster_edge_betweenness.rds")
V(graph_releases)[clust_releases$names]$cluster <- clust_releases$membership
table(clust_releases$membership)
# Should I remove the releases now?

# Find most authoritative performers of each community
qty_authoritative <- 3

df_authoritative <- tibble(
  cluster = V(graph_releases)$cluster,
  type_performer = V(graph_releases)$type_performer,
  name_performer = V(graph_releases)$name_node,
  role_primary = V(graph_releases)$role_primary,
  id_performer = V(graph_releases)$name,
  qty_collection_items = V(graph_releases)$qty_collection_items,
  qty_edges = V(graph_releases)$qty_edges 
)

df_authoritative %<>%
  arrange(desc(qty_edges)) %>% 
  group_by(cluster) %>% 
  mutate(idx_row_qty_edges = row_number()) %>% 
  ungroup()

df_test <- df_authoritative %>% 
  filter(!is.na(type_performer)) %>% 
  filter(qty_collection_items > 0 ) %>% 
  #filter(!is.infinite(qty_collection_items)) %>% 
  #filter(idx_row_qty_edges <= qty_authoritative) %>% 
  arrange(cluster)

# Get all cluster hierarchies ----
name_table <- "node_community_hierarchy"
db_discogs <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))

# Find the minimum of communities
res_cut <- cut_at(clust_releases, no = 1)
qty_communities_min <- max(res_cut)

# Getting all community memberships from top to bottom of the hierarchy
qty_communities <- qty_communities_min
idx_step <- 1
lst_communities <- list()
while(qty_communities < length(clust_releases$membership)){
  
  no_communities <- qty_communities_min + idx_step
  id_communities <- cut_at(clust_releases, no = no_communities)
  qty_communities <- max(id_communities)
  
  df_communitiy_membership <- tibble(
    id_node = V(graph_releases)$name,
    idx_step = rep(idx_step, length(id_communities)),
    id_community = id_communities
  )
  
  has_table <- dbExistsTable(db_discogs, name_table)
  dbWriteTable(db_discogs, name_table, df_communitiy_membership, overwrite = !has_table, append = has_table)
  
  idx_step <- idx_step + 1
}
db_conn <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))

res <- dbSendQuery(db_conn, paste0("SELECT * FROM node_community_hierarchy"))
df_result <- dbFetch(res)



dendro_clusters <- as.dendrogram(clust_releases)
dendro_clusters %>% nnodes

hclust_clusters <- as.hclust(clust_releases)

phylo_clusters <- as_phylo(clust_releases)
phylo_clusters$edge
plot(phylo_clusters, type = "fan")

res_cut <- cut_at(clust_releases, no = 1)
qty_communities_min <- max(res_cut)
table(res_cut)

qty_communities_step <- qty_communities_min + 5
res_cut <- cut_at(clust_releases, no = qty_communities_step)
table(res_cut)

qty_communities_step <- qty_communities_step + 5
res_cut <- cut_at(clust_releases, no = qty_communities_step)
table(res_cut)


qty_communities_step <- length(res_cut)
res_cut <- cut_at(clust_releases, no = qty_communities_step)
table(res_cut)

# Declared to be old, but still useful (2021-08-12)

# Clustering of network nodes to find communities within the graph ----
clust <- cluster_fast_greedy(graph_reduced)
#clust <- cluster_edge_betweenness(graph_reduced, directed = FALSE)
#write_rds(clust, "cluster_edge_betweenness.rds")

# Assigning the results of the clustering back to the network nodes
V(graph_reduced)[names(membership(clust))]$community <- membership(clust)

# Checks: number of nodes in the network, and only those that represent artists
table(V(graph_reduced)$community)
table(V(graph_reduced)[V(graph_reduced)$type_node == "performer"]$community)

# Select a community and plot it
idx_community <- 1
graph_community <- delete_vertices(graph_reduced, V(graph_reduced)[V(graph_reduced)$community != idx_community])
bool_performers <- V(graph_community)$type_node == "performer"
graph_performer_community <- delete_vertices(graph_community, V(graph_community)[!bool_performers])

plot(graph_performer_community)

bool_releases <- V(graph_community)$type_node != "performer"
graph_releases_community <- delete_vertices(graph_community, V(graph_community)[!bool_releases])
plot(graph_releases_community)


# Doing some stuff I'll keep around, but not sure what it does yet
library(RColorBrewer)
qty_colors <- length(unique(V(graph_reduced)$community))
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(qty_colors)
V(graph_reduced)$label <- V(graph_reduced)$name_node
V(graph_reduced)$color <- mycolors[V(graph_reduced)$community]
plot(clust, graph_reduced)

df_community <- tibble(
  community = V(graph_reduced)$community,
  name_artist = V(graph_reduced)$name_node
)

# qty_node_edges <- degree(graph_reduced)
# table(qty_node_edges)
# idx_nodes <- names(qty_node_edges[qty_node_edges > 50])
# V(graph_all)[idx_nodes]$name_node

# Get sub-graphs of interconnected nodes
lst_sub_graph <- decompose(graph_reduced)
qty_nodes_sub_graph <- lengths(lapply(lst_sub_graph, '[['))
sub_graph_reduced <- lst_sub_graph[[4]]

idx_performers <- V(sub_graph_reduced)$type_node == "performer"
V(sub_graph_reduced)[idx_performers]$label <- V(sub_graph_reduced)[idx_performers]$name_node

plot(sub_graph_reduced)

between_sub_graph <- betweenness(sub_graph_reduced)
hist(between_sub_graph)
ntile(between_sub_graph, n = 10)
library(RColorBrewer)
my_orange <- brewer.pal(n = 10, "RdYlBu")

V(sub_graph_reduced)$color <- my_orange[ntile(between_sub_graph, n = 10)]
plot(sub_graph_reduced)


score <- authority_score(sub_graph_reduced)$vector
ntile(score, n = 10)
library(RColorBrewer)
my_orange <- brewer.pal(n = 10, "RdYlBu")

V(sub_graph_reduced)$color <- my_orange[ntile(between_sub_graph, n = 10)]
plot(sub_graph_reduced)

for(sub_graph in lst_sub_graph[which(qty_artists_sub_graph == 1)]){
  print(V(sub_graph)$name_node)
  print(V(sub_graph)$name)
}



df_edges %>% 
  anti_join(df_nodes, by = c("from" = "id_node")) %>% 
  inner_join(df_collection_artists, by = c("from" = "id_release"))
