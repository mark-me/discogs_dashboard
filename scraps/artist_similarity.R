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
  select(id_artist) %>% 
  mutate(id_artist = paste0("a_", id_artist),
         qty_releases = 1)

df_nodes_artists <- bind_rows(
  df_artists %>% 
    select(id_artist, name_artist, url_thumbnail) %>% 
    mutate(type_node = "artist",
           id_artist = paste0("a_", id_artist)),
  df_members %>% 
    select(id_artist = id_member, name_artist = name_member, url_thumbnail, type_node),
  df_groups  %>% 
    select(id_artist = id_group,  name_artist = name_group, url_thumbnail, type_node),
  df_aliases %>% 
    select(id_artist = id_alias,  name_artist = name_alias, url_thumbnail, type_node),
)

df_nodes_artists %<>% 
  left_join(df_releases, by = "id_artist") %>% 
  group_by(id_artist, name_artist) %>% 
  summarise(url_thumbnail = max(url_thumbnail),
            type_node     = max(type_node, na.rm = TRUE),
            qty_collection_items = sum(qty_releases, na.rm = TRUE)) %>% 
  ungroup()

df_edges_artists <- bind_rows(
  df_members  %>% select(from = id_artist, to = id_member, type_edge),
  df_groups   %>% select(from = id_group, to = id_artist, type_edge),
  df_aliases  %>% select(from = id_artist, to = id_alias, type_edge)
)  %>% unique()

graphs_artists <- graph_from_data_frame(df_edges_artists, vertices = df_nodes_artists, directed = FALSE)


lst_artist <- list()
qty_artists <- length(V(graphs_artists))
pb <- txtProgressBar(min = 0, max = qty_artists, style = 3)
for(i in 1:qty_artists){
  
  setTxtProgressBar(pb, value = i)
  lst_artist[[i]] <- tibble(
    id_artists_similar = V(graphs_artists)$name,
    id_artist = rep(V(graphs_artists)[[i]]$name, length(V(graphs_artists))),
    qty_sim = all_shortest_paths(graphs_artists, from = V(graphs_artists)[[i]])$nrgeo  
  )
}

df_artists_similar <- bind_rows(lst_artist) %>% 
  mutate(qty_sim = ifelse(id_artist == id_artists_similar, 0, qty_sim)) %>% 
  group_by(id_artist) %>% 
  mutate(perc_sim = qty_sim/sum(qty_sim),
         qty_artist = sum(qty_sim)) %>% 
  ungroup() 

db_discogs <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))
dbWriteTable(db_discogs, "artists_similar", df_artists_similar, overwrite = TRUE)

df_artists_similar %<>% 
  pivot_wider(id_cols = id_artist, names_from = id_artists_similar, values_from = perc_sim)



V(graphs_artists)$name
