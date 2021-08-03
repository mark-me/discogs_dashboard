library(visNetwork)
library(igraph)

df_members <- df_artists %>% 
  select(id_artist, name_artist) %>% 
  inner_join(df_artist_members, by = "id_artist") %>% 
  select(id_artist, name_artist, id_member, name_member, url_thumbnail) %>% 
  mutate(id_artist = paste0("a_", id_artist),
         id_member = paste0("a_", id_member),
         type_edge = "group_member",
         type_node = "group")

df_groups <- df_artists %>% 
  select(id_artist, name_artist) %>% 
  inner_join(df_artist_groups, by = "id_artist") %>% 
  select(id_artist, name_artist, id_group, name_group, url_thumbnail) %>% 
  mutate(id_artist = paste0("a_", id_artist),
         id_group = paste0("a_", id_group),
         type_edge = "group_member",
         type_node = "artist")

df_aliases <- df_artists %>% 
  select(id_artist, name_artist) %>% 
  inner_join(df_artist_aliases, by = "id_artist") %>% 
  select(id_artist, name_artist, id_alias, name_alias, url_thumbnail) %>% 
  mutate(id_artist = paste0("a_", id_artist),
         id_alias  = paste0("a_", id_alias),
         type_edge = "alias",
         type_node = "alias")

df_releases <- df_collection_items %>% 
  left_join(df_collection_artists, by = "id_release") %>% 
  select(id = id_release, name = title, id_artist, url_thumbnail = url_thumb) %>% 
  mutate(id = paste0("r_", id),
         id_artist = paste0("a_", id_artist),
         type_edge = "release",
         type_node = "release")

df_nodes_artists <- bind_rows(
  df_artists %>% 
    select(id = id_artist, name = name_artist, url_thumbnail) %>% 
    mutate(type_node = "artist",
           id = paste0("a_", id)),
  df_members %>% 
    select(id = id_member, name = name_member, url_thumbnail, type_node),
  df_groups  %>% 
    select(id = id_group,  name = name_group, url_thumbnail, type_node),
  df_aliases %>% 
    select(id = id_alias,  name = name_alias, url_thumbnail, type_node),
  df_releases %>% 
    select(id,  name, url_thumbnail, type_node)
)

df_nodes_artists %<>%
  group_by(id, name) %>%
  summarise(image = first(url_thumbnail),
            type_node = max(type_node)) %>%
  ungroup() %>%
  mutate(is_group = id %in% as.numeric(df_members$id_artist)) %>%
  mutate(in_collection = id %in% df_artists$id_artist | type_node == "release") %>%
  rename(name_artist = name)

df_edges_artists <- bind_rows(
  df_members  %>% select(from = id_artist, to = id_member, type_edge),
  df_groups   %>% select(from = id_group, to = id_artist, type_edge),
  df_releases %>% select(from = id_artist, to = id, type_edge)
)  %>% unique()

df_id_qty_edges <- bind_rows(
  df_edges_artists %>% 
    select(id = from) %>% 
    group_by(id) %>% 
    summarise(qty_edges = n()) %>% 
    ungroup(),
  df_edges_artists %>% 
    select(id = to) %>% 
    group_by(id) %>% 
    summarise(qty_edges = n()) %>% 
    ungroup()
) %>% 
  group_by(id) %>% 
  summarise(qty_edges = sum(qty_edges)) %>% 
  ungroup()

df_nodes_artists %<>% 
  left_join(df_id_qty_edges, by = "id") %>% 
  filter(qty_edges > 1 | in_collection)

df_edges_artists %<>%
  filter(from %in% df_nodes_artists$id & to %in% df_nodes_artists$id)

graph <- graph_from_data_frame(df_edges_artists, vertices = df_nodes_artists, directed = FALSE)
V(graph)$color <- ifelse(V(graph)$in_collection == TRUE, "orange", "grey")

lst_ego_graphs <- decompose(graph)

vec_length <- c()
for(i in 1:length(lst_ego_graphs)){
  qty_artists <- length(V(lst_ego_graphs[[i]]))
  vec_length <- c(vec_length, qty_artists)
}
lst_ego_graphs[vec_length == 1] <- NULL
vec_length <- vec_length[vec_length != 1]

vec_length

i_graph <- 1
ego_graph <- lst_ego_graphs[[i_graph]]
network <- list(
  df_nodes = igraph::as_data_frame(ego_graph, what = "vertices") %>% 
    rename(id = name, label = name_artist) %>% 
    mutate(shape = ifelse(type_node == "release", "image", "circularImage")),
  df_edges = igraph::as_data_frame(ego_graph, what = "edges")
)




clust <- cluster_edge_betweenness(ego_graph, directed = FALSE)
V(ego_graph)[names(membership(clust))]$community <- membership(clust)

table(V(ego_graph)$community)

library(RColorBrewer)
qty_colors <- length(unique(V(ego_graph)$community))
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(qty_colors)
V(ego_graph)$label <- V(ego_graph)$name_artist
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


