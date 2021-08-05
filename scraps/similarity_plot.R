library(ggrepel)

df_artist_mds %>% 
  left_join(df_nodes_artists, by = "id_artist") %>% 
ggplot(aes(x = x, y = y)) +
  geom_jitter() +
  geom_label_repel(aes(label = name_artist), max.overlaps = 149) 
