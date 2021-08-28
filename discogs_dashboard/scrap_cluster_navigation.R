file_db <- '~/Development/Datasets/discogs_dashboard/discogs.sqlite'
file_cluster_result <- 'cluster_edge_betweenness.rds'

nw_performer_releases <- get_performer_master_network(file_db)
res_clustering <- get_performer_clusters(nw_performer_releases, file_cluster_result, do_cluster_calculation = FALSE)

df_clusters <- tibble(
  id_node    = res_clustering$names,
  id_cluster = res_clustering$membership
)

nw_performer_releases$df_nodes %<>%
  left_join(df_clusters, by = "id_node")

df_non_clustered <- nw_performer_releases$df_nodes %>% 
  filter(is.na(id_cluster))

df_non_clustered %<>% 
  select(-id_cluster) %>% 
  inner_join(select(nw_performer_releases$df_edges, id_node = to, id_artist = from), 
             by = "id_node") %>% 
  inner_join(select(nw_performer_releases$df_nodes, id_artist = id_node, id_cluster),
             by = "id_artist") %>% 
  select(id_node, id_cluster)


qty_steps <- length(clust_performer_releases$merges)/2
id_communities <- cut_at(clust_performer_releases, steps = qty_steps)
