library(ggrepel)

df_test <- df_artist_mds %>% 
  left_join(df_nodes_artists, by = "id_artist") 
ggplot(df_test, aes(x = x, y = y)) +
  geom_jitter() +
  geom_label_repel(aes(label = name_artist), max.overlaps = 400) 


library(Rtsne)

fit_tsne <- Rtsne(dist_artists,
                  is_distance = TRUE,
                  check_duplicates = FALSE, 
                  pca = FALSE, 
                  perplexity = 120, 
                  theta = 0.5, 
                  dims = 2)

df_tsne_vars <- tibble(id_artist = mat_distances$id_artist,
                       x = fit_tsne$Y[, 1],
                       y = fit_tsne$Y[, 2])
df_test <- df_tsne_vars %>% 
  left_join(df_nodes_artists, by = "id_artist") 

ggplot(df_test, aes(x = x, y = y)) +
  geom_jitter() +
  geom_label_repel(aes(label = name_artist), max.overlaps = 400) 


df_test <- df_artists_similar %>% 
  left_join(select(df_nodes_artists, id_artist, name_artist), 
            by = "id_artist") %>% 
  left_join(select(df_nodes_artists, id_artists_similar = id_artist, name_artists_similar = name_artist),
            by = "id_artists_similar")
  
