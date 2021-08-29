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
  select(id_node, id_cluster_derived = id_cluster)

nw_performer_releases$df_nodes %<>%
  left_join(df_non_clustered, by = "id_node") %>% 
  mutate(id_cluster = ifelse(is.na(id_cluster), id_cluster_derived, id_cluster)) %>% 
  select(-id_cluster_derived)

get_cluster <- function(res_clustering, id_step = NA, id_cluster_selected = NA){
  
  # If no step is given, the most aggregated level is shown
  if(is.na(id_step)){
    
    id_step    <- length(res_clustering$merges)/2
    id_cluster <- cut_at(res_clustering, steps = id_step)
    is_visible <- rep(TRUE, length(id_cluster))
    
  } else {
    
    # Set the nodes to visible that fall in the selected cluster
    is_visible <- cut_at(res_clustering, steps = id_step) == id_cluster_selected
    
    # Only drill down if there is more than one node in the cluster
    if(sum(is_visible) > 1){
      
      # Drill down the hierarchy until there are at least 6 clusters or the maximum number of nodes is reached
      qty_clusters_min <- ifelse(sum(is_visible) < 6, sum(is_visible), 6)
      qty_clusters <- 0
      id_step <- id_step - 1
      while(qty_clusters < qty_clusters_min){
        
        id_cluster   <- cut_at(res_clustering, steps = id_step)
        qty_clusters <- length(unique(id_cluster[is_visible]))
        id_step      <- id_step - 100 # Taking the number of steps by 100 to speed up the search process
      }      
    } else {
      id_cluster   <- cut_at(res_clustering, steps = id_step)
    }

  }
  
  lst_search <- list(
    id_step    = id_step,
    df_cluster = tibble(
      id_nodes   = res_clustering$names,
      id_cluster = id_cluster,
      is_visible = is_visible
    )
  )
  
  return(lst_search)
}

lst_searches <- list()

i <- 1
lst_searches[[i]] <- get_cluster(res_clustering, id_step = NA, id_cluster_selected = NA)
i <- 2
lst_searches[[i]] <- get_cluster(res_clustering, id_step = lst_searches[[i-1]]$id_step, id_cluster_selected = 3)
table(lst_searches[[i]]$df_cluster$id_cluster[lst_searches[[i]]$df_cluster$is_visible])
i <- 3
lst_searches[[i]] <- get_cluster(res_clustering, id_step = lst_searches[[i-1]]$id_step, id_cluster_selected = 225)
table(lst_searches[[i]]$df_cluster$id_cluster[lst_searches[[i]]$df_cluster$is_visible])
i <- 4
lst_searches[[i]] <- get_cluster(res_clustering, id_step = lst_searches[[i-1]]$id_step, id_cluster_selected = 9645)

id_step <- lst_searches[[2]]$id_step
id_cluster_selected <- 225
sum(is_visible)
