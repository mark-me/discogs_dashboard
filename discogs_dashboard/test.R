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
plot_network(lst_search_results[[i]]$nw_cluster)

df_performers <- get_cluster_performers(df_network_nodes = lst_network$nw_performer_releases$df_nodes,
                                        df_cluster_ids   = lst_search_results[[i]]$df_cluster_ids)
df_releases   <- get_cluster_releases(df_network_nodes = lst_network$nw_performer_releases$df_nodes,
                                      df_cluster_ids   = lst_search_results[[i]]$df_cluster_ids)

i <- 2
lst_search_results[[i]] <- get_clustered_network(lst_network, lst_search_results, id_cluster_selected = 3)
plot_network(lst_search_results[[i]]$nw_cluster)

df_test_nodes <- lst_search_results[[i]]$nw_cluster$df_nodes

i <- 3
lst_search_results[[i]] <- get_clustered_network(lst_network, lst_search_results, id_cluster_selected = 127)
plot_network(lst_search_results[[i]]$nw_cluster)  

i <- 4
lst_search_results[[i]] <- get_clustered_network(lst_network, lst_search_results, id_cluster_selected = 771)
plot_network(lst_search_results[[i]]$nw_cluster)  

i <- 5
lst_search_results[[i]] <- get_clustered_network(lst_network, lst_search_results, id_cluster_selected = 2312)
plot_network(lst_search_results[[i]]$nw_cluster)  
