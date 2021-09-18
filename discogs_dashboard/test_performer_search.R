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

# Start function ----

name_performer <- "Frank Sinatra"

search_cluster_performer <- function(lst_network, name_performer){
  
  # Uncomplicate data referencing
  res_clustering <- lst_network$res_clustering
  nw <- lst_network$nw_performer_releases
  
  # Make graph of network  
  graph_network <- graph_from_data_frame(d = nw$df_edges, vertices = nw$df_nodes, directed = FALSE)
  is_node_perfomer <- V(graph_network)$name_node == name_performer & V(graph_network)$type_node == "performer"
  id_node_perfomer <- V(graph_network)[is_node_perfomer]$name
  
  # Drill down the hierarchy until there is closest to _qty_in_cluster_optimum_ performers in a cluster
  qty_in_cluster_optimum <- 10
  qty_in_cluster <- 0       # Number of performers in a cluster
  id_step <- 1
  stop <- FALSE
  step_size <- 7000          # Initial step size through the hierarchy to speed search up
  distance_optimum <- -99999 # Distance to _qty_in_cluster_optimum_
  crossed_zero <- FALSE      # Indicates overshooting optimum cluster size
  while(!stop){
    
    # Assign clustering to nodes
    id_cluster <- cut_at(res_clustering, steps = id_step)
    V(graph_network)[res_clustering$names]$id_cluster <- id_cluster       
    id_cluster_performer <- V(graph_network)[id_node_perfomer]$id_cluster # Get performer's cluster id
    
    # Find the number of performers in the same cluster as the searched performer
    is_perfomers         <- V(graph_network)$type_node == "performer"
    in_performer_cluster <- V(graph_network)$id_cluster == id_cluster_performer
    qty_in_cluster       <- length(V(graph_network)[is_perfomers & in_performer_cluster])
    
    # If the distance to the optimum switches from positive to negative or reverse ....
    crossed_zero     <- (distance_optimum * (qty_in_cluster - qty_in_cluster_optimum)) < 0
    distance_optimum <- qty_in_cluster - qty_in_cluster_optimum

    message(sprintf("Distance: %s", distance_optimum))
    message(sprintf("Step size: %s", step_size))
    message(sprintf("Step", id_step))
    
    prev_step_size <- step_size
    # take smaller steps
    if(distance_optimum > 0 & crossed_zero){
      step_size <- -1 * abs(round(step_size/4))
      step_size <- ifelse(step_size == 0, -1, step_size)
    } else if(distance_optimum < 0 & crossed_zero){
      step_size <- abs(round(step_size/4))
      step_size <- ifelse(step_size == 0, 1, step_size)
    } 
    
    # Stop processing if process keeps swinging around a 0 distance from optimum (i.e. -1, +1)
    if(step_size + prev_step_size == 0){
      stop <- TRUE
    }
    
    id_step <- id_step + step_size  # Taking the number of steps by 100 to speed up the search process
  }    
  
  lst_search <- list(
    id_step_hierarchy = id_step,
    df_cluster = tibble(id_node = res_clustering$names,
                        id_cluster = id_cluster,
                        is_cluster_visible = id_cluster == id_cluster_performer)
  )
  
  return(lst_search)
}

get_performer_network <- function(lst_network, name_performer){
  
  nw <- lst_network$nw_performer_releases
  
  lst_search_result <- search_cluster_performer(lst_network, name_performer) 
  
  # Add cluster id's to the nodes
  nw$df_nodes %<>% left_join(lst_search_result$df_cluster, by = "id_node") # Add cluster ID's to nodes
  
  # Set single connecting releases cluster to the performer's cluster ID 
  nw <-move_single_connecting_releases_to_cluster(nw)
  nw <- add_edge_count(nw)  # Add count of edges per node
  # If a cluster contains only releases that connect other clusters duplicate the nodes across clusters
  nw <- copy_multiple_connecting_releases_to_clusters(nw)
  nw <- add_performer_image(nw)
  nw <- add_most_authoritative_names(nw, qty_authoritative = 3) # Add _qty_authoritative_ most authoritative performers as a cluster description
  nw <- add_cluster_statistics(nw)         # Add cluster statistics
  nw_contracted <- aggregate_network(nw)   # Contract network to clusters
  
  # Temp fix: replacing node id's with cluster id's
  nw_contracted$df_edges %<>% 
    left_join(select(nw_contracted$df_nodes, id, id_cluster),
              by = c("from" = "id")) %>% 
    mutate(from = id_cluster) %>% 
    select(-id_cluster) %>% 
    left_join(select(nw_contracted$df_nodes, id, id_cluster),
              by = c("to" = "id")) %>% 
    mutate(to = id_cluster) %>% 
    select(-id_cluster) 
  
  nw_contracted$df_nodes %<>%
    mutate(id = id_cluster) %>% 
    mutate(label = name_authoritative)
  
  # Compile return object
  lst_search_result <- list(
    id_step_hierarchy = lst_search_result$id_step_hierarchy,
    df_cluster_ids    = nw$df_nodes %>% select(id_node, id_cluster, is_cluster_visible),
    nw_cluster        = nw_contracted
  )
  return(lst_search_result)
}

lst_results <- get_performer_network(lst_network, name_performer = "Frank Sinatra")
plot_network(lst_results$nw_cluster)
