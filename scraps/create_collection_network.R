library(httr)		# Library for scraping web data
library(rjson)
library(tidyverse)
library(magrittr)
library(yaml)
library(RSQLite)
library(igraph)

config <- read_yaml("config.yml")
source("data-prep/get_network_data_functions.R")

lst_network <- get_release_network()

graph_all <- graph_from_data_frame(lst_network$df_edges, 
                                   vertices = lst_network$df_nodes, 
                                   directed = FALSE)

# Removing irrelevant nodes from the network
performers_with_release <- V(graph_all)$qty_releases > 0
qty_node_edges <- degree(graph_all)
multiple_edges <- V(graph_all)$name %in% names(qty_node_edges[qty_node_edges > 1])
keep <- performers_with_release | multiple_edges
remove <- !keep
graph_reduced <- delete_vertices(graph_all, V(graph_all)[remove])
# Remove mutual affirming edges
graph_reduced <- simplify(graph_reduced)



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
V(graph_community)[!bool_performers]$label <- ""
graph_community <- delete_vertices(graph_community, V(graph_community)[!bool_performers])

plot(graph_community)

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
