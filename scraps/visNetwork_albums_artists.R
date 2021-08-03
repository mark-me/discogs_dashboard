library(visNetwork)
library(igraph)

df_nodes_artist <- df_artists %>% 
  mutate(id_artist = as.numeric(id_artist)) %>% 
  left_join(df_artist_images, by = "id_artist") %>%
  mutate(id_artist = as.character(id_artist)) %>% 
  arrange(id_artist, type_image) %>% 
  group_by(id_artist, name_artist) %>% 
  summarise(image = first(url_image_150)) %>% 
  ungroup() %>% 
  rename(id = id_artist,
         name = name_artist) %>% 
  mutate(type = "artist")

df_nodes_album <- df_collection_items %>% 
  select(id = id_release,
         name = title,
         image = url_thumb) %>% 
  mutate(type = "release")

df_nodes <- bind_rows(df_nodes_artist, df_nodes_album)

df_edges <- df_collection_artists %>% 
  mutate(from = as.character(id_artist),
         to = as.character(id_release)) %>% 
  select(from, to)

df_edges %<>% 
  filter(from %in% df_nodes$id & to %in% df_nodes$id)

graph <- graph_from_data_frame(df_edges, vertices = df_nodes)

lst_ego_graphs <- decompose(graph)
lst_length <- lapply

vec_length <- c()
for(i %in% 1:length(lst_ego_graphs)){
  print(i)
  # vec_length <- c(vec_length, lst_ego_graphs[[i]])
}

plot(lst_ego_graphs[[9]])



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

plot_network(network)
