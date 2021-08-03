library(ggrepel)

table(df_collection_genres$name_genre)
table(df_collection_items$qty_genres)
table(df_collection_styles$name_style)
table(df_collection_items$qty_styles)

df_collection_genre <- rbind(
  df_collection_items %>% 
    left_join(df_collection_genres, by = "id_release") %>% 
    select(id_master, id_release, name_genre),
  df_collection_items %>% 
    left_join(df_collection_styles, by = "id_release") %>% 
    select(id_master, id_release, name_genre = name_style)
)
  
df_collection_genre %<>% 
  mutate(id_release = as.integer(id_release)) %>% 
  left_join(df_collection_artists, by = "id_release") %>% 
  select(id_artist, name_artist,id_master, name_genre) %>% 
  unique() %>% 
  mutate(is_genre = as.integer(1)) 

# table((df_collection_genre %>% 
#   group_by(id_artist) %>% 
#   summarise(qty_genres = sum(is_genre)) %>% 
#   ungroup())$qty_genres)

df_collection_genre %<>% 
  group_by(id_artist, name_artist, name_genre) %>% 
  summarise(qty_genres = sum(is_genre)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c("id_artist", "name_artist"),
              names_from = name_genre,
              values_from = qty_genres,
              values_fill = 0) 

dist_genres <- dist(df_collection_genre, method = "manhattan")
mat_distance <- as.matrix(dist_genres)
mds_genres <- cmdscale(mat_distance)

df_mds_genres <- cbind(df_collection_genre,
                       x_artist = mds_genres[,1], 
                       y_artist = mds_genres[,2]) %>% 
  select(ends_with("_artist"))

ggplot(df_mds_genres, aes(x_artist, y_artist)) +
  geom_jitter() +
  geom_label_repel(aes(label = name_artist)) +
  scale_color_continuous() +
  scale_fill_continuous()
  