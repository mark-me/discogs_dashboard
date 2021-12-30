library(tidyverse)
library(magrittr)
library(RSQLite)
library(yaml)
library(scales)
library(ggimage)
library(visNetwork)
source("discogs_dashboard/get_network_data_functions.R")
source("discogs_dashboard/cluster_navigation.R")

config <- read_yaml("config.yml")
file_db <- paste0(config$db_location, "/discogs.sqlite")

lst_network <- list(
  res_clustering = read_rds(config$file_cluster_results),
  nw_performer_releases = get_performer_master_network(file_db)
)

lst_search_results <- list()

i <- 1
lst_search_results[[i]] <- get_clustered_network(lst_network, lst_search_results = NA, id_cluster_selected = NA)
# lst_search_results[[i]]$nw_cluster$df_nodes %<>%
#   mutate(id_cluster = paste0(lst_search_results[[i]]$id_step_hierarchy, "_", id_cluster)) 

plot_network(lst_search_results[[i]]$nw_cluster)


# Start with a top layer (closest to the root), add the nodes and the information to a data frame called 'df_zoom', set as a 0 as the parent id
lst_search <- get_clustered_network(lst_network, lst_search_results = NA, id_cluster_selected = NA)
df_zoom <- lst_search$nw_cluster$df_nodes %>% 
  mutate(id_parent = "0",
         id_zoom = 1)

# For each node in the layer with id_node:
#   find the underlying nodes
#   Add the current layer to the data frame zoom with the parent id, set the parent id to id_node

id_clusters <- unique(df_zoom$id_cluster)
i <- 2
for(id_cluster in id_clusters){
  
  lst_search <- get_clustered_network(lst_network, lst_search_results, id_cluster_selected = id_cluster)
  df_zoom <- bind_rows(
    df_zoom,
    lst_search$nw_cluster$df_nodes %>% 
      mutate(id_parent = as.character(id_cluster),
             id_zoom = i)
  )
}
plot_network(lst_search_results[[i-11]]$nw_cluster)

