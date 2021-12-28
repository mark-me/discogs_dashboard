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

id_clusters <- unique(lst_search_results[[i]]$nw_  $id_cluster)
i <- 2
for(id_cluster in id_clusters){
  
  lst_search_results[[i]] <- get_clustered_network(lst_network, lst_search_results, id_cluster_selected = id_cluster)
  i <- i + 1
}
plot_network(lst_search_results[[i-11]]$nw_cluster)

