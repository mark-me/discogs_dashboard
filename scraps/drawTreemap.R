library(SysbioTreemaps)

df <- data.frame(
  A = rep(c("a", "b", "c"), each = 15),
  B = sample(letters[4:12], 45, replace = TRUE),
  C = sample(10:100, 45)
)

tm <- voronoiTreemap(
  data = df,
  levels = c("A", "B", "C"),
  cell_size = "C",
  shape = "rounded_rect"
)

drawTreemap(tm, title = "treemap 1", 
            color_type = "categorical", color_level = 1, 
            layout = c(1,3), position = c(1, 1))

drawTreemap(tm, title = "treemap 2",
            color_type = "categorical", color_level = 2, border_size = 3,
            add = TRUE, layout = c(1,3), position = c(1, 2))

drawTreemap(tm, title = "treemap 3",
            color_type = "cell_size", color_level = 3,
            color_palette = heat.colors(10),
            border_color = grey(0.4), label_color = grey(0.4),
            add = TRUE, layout = c(1,3), position = c(1, 3),
            title_color = "black")

df_nodes <- lst_search_results[[1]]$nw_cluster$df_nodes

df_nodes %<>%
  mutate(qty_collection_log = log(qty_collection) + 1)

tm <- voronoiTreemap(
  data = df_nodes,
  levels = c("id_cluster", "label"),
  cell_size = "qty_collection_log",
  shape = "rounded_rect"
)

drawTreemap(tm, 
            color_type = "categorical", color_level = 1,
            label_size = rep(3,nrow(df_nodes)))


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


get_network <- function(lst_network, lst_search_results = NA, id_cluster_selected = NA){
  
  nw <- lst_network$nw_performer_releases
  
  if(is.na(lst_search_results)){
    # Get least number of clusters if no previous search results are provided
    lst_search_result <- get_clusters(lst_network$res_clustering)  
  } else {
    # Get clustering based on the previous aggregation level and a chosen node within that aggregation
    lst_search_result <- get_clusters(res_clustering = lst_network$res_clustering,
                                      id_step = lst_search_results[[length(lst_search_results)]]$id_step, 
                                      id_cluster_selected = id_cluster_selected) 
  }
  
  nw$df_nodes %<>% left_join(lst_search_result$df_cluster, by = "id_node") # Add cluster ID's to nodes
  
  return(nw)
}


lst_search_results <- list()

i <- 1
lst_search_results[[i]] <- get_network(lst_network, lst_search_results = NA, id_cluster_selected = NA)


df_nodes <- lst_search_results[[1]]$df_nodes %>%
  filter(type_node == "performer") %>% 
  filter(qty_collection_items > 0) %>% 
  mutate(qty_collection_log = log(qty_collection_items) + 1) 

tm <- voronoiTreemap(
  data = df_nodes,
  levels = c("id_cluster", "name_node"),
  cell_size = "qty_collection_log",
  shape = "rounded_rect"
)

drawTreemap(tm, 
            color_type = "categorical", color_level = 1,
            label_size = df_nodes$qty_collection_log*10)

i <- 2
lst_search_results[[i]] <- get_network(lst_network, lst_search_results, id_cluster_selected = 1)

df_nodes <- lst_search_results[[i]]$nw_cluster$df_nodes

df_nodes %<>%
  filter(qty_collection > 0) %>% 
  mutate(qty_collection_log = log(qty_collection) + 1)

tm <- voronoiTreemap(
  data = df_nodes,
  levels = c("id_cluster", "label"),
  cell_size = "qty_collection_log",
  shape = "rounded_rect"
)

drawTreemap(tm, 
            color_type = "categorical", color_level = 1,
            label_size = rep(3,nrow(df_nodes)))

i <- 3
lst_search_results[[i]] <- get_clustered_network(lst_network, lst_search_results, id_cluster_selected = 127)

df_nodes <- lst_search_results[[i]]$nw_cluster$df_nodes

df_nodes %<>%
  filter(qty_collection > 0) %>% 
  mutate(qty_collection_log = log(qty_collection) + 1)

tm <- voronoiTreemap(
  data = df_nodes,
  levels = c("id_cluster", "label"),
  cell_size = "qty_collection_log",
  shape = "rounded_rect"
)

drawTreemap(tm, 
            color_type = "categorical", color_level = 1,
            label_size = rep(3,nrow(df_nodes)))
