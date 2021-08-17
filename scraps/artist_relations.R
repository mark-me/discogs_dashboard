library(visNetwork)
library(igraph)

library(RColorBrewer)
qty_colors <- length(unique(V(ego_graph)$community))
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(qty_colors)
V(ego_graph)$label <- V(ego_graph)$name_node
V(ego_graph)$color <- mycolors[V(ego_graph)$community]
plot(ego_graph)

df_community <- tibble(
  community = V(ego_graph)$community,
  name_artist = V(ego_graph)$name_artist
)
id_community <- c(3)
graph_sub <- subgraph(ego_graph, V(ego_graph)[V(ego_graph)$community %in% id_community])

network <- list(
  df_nodes = igraph::as_data_frame(graph_sub, what = "vertices") %>% 
    rename(id = name) %>% 
    mutate(shape = ifelse(type_node == "release", "image", "circularImage")),
  df_edges = igraph::as_data_frame(graph_sub, what = "edges")
)

plot(graph_sub)
plot_network(network)
plot(lst_ego_graphs[[1]])

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


