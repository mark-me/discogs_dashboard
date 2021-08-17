library(httr)		# Library for scraping web data
library(rjson)
library(tidyverse)
library(magrittr)
library(yaml)
library(RSQLite)
library(igraph)
#source("data-prep/load_discogs_all.R")

# Calculate release similarity for sorting releases ----
calculate_release_similarity <- function(recalculate = FALSE){
  
}

# Calculate shortest distances between performers ----
calculate_performer_similarity <- function(recalculate = FALSE){

  graph_releases <- get_graph_releases()

  if(recalculate){
 
    # Get sub-graphs of interconnected nodes
    lst_sub_graph <- decompose(graph_releases)
    # qty_nodes_sub_graph <- lengths(lapply(lst_sub_graph, '[[') ) # Am I doing anything with this? Expensive statement
    
    # Setting up parallel processing
    qty_cores <- parallel::detectCores() - 2
    calc_cluster <- parallel::makeCluster(qty_cores, type = "FORK")
    doParallel::registerDoParallel(cl = calc_cluster)
    
    lst_artist <- list()
    idx_artists <- 0
    for(sub_graph in lst_sub_graph){
      
      idx_collection_artists <- V(sub_graph)[V(sub_graph)$in_collection]$name
      # Calculate shortest distances between all artists in a sub-graph
      lst_sub_artists <- foreach(
        i = idx_collection_artists
      ) %dopar% {
        tibble(id_artists_similar = V(sub_graph)$name,
               id_artist = rep(V(sub_graph)[[i]]$name, 
                               length(V(sub_graph))),
               qty_hops  = all_shortest_paths(sub_graph, 
                                              from = V(sub_graph)[[i]])$nrgeo)
      }
      lst_artist <- c(lst_artist, lst_sub_artists)
    }
    parallel::stopCluster(cl = calc_cluster) # Stop parallel cluster
      

    df_artists_similar <- bind_rows(lst_artist) %>% 
      #rename(qty_hops = qty_sim) %>% 
      mutate(qty_hops = ifelse(id_artist == id_artists_similar, 0, qty_hops)) 
    
    # Store distances in the database
    db_discogs <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))
    dbWriteTable(db_discogs, "artist_distances", df_artists_similar, overwrite = TRUE)
    
  } else {
    
    # Read similarities from database
    db_discogs <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))
    df_artists_similar <- dbReadTable(db_discogs, "artist_distances")
  }

  # Removing artists which are not in the user's collection
  df_artists_similar %<>% 
    #filter(id_artists_similar != id_artist) %>% 
    semi_join((df_node_artist_collection %>% filter(in_collection)), 
              by = "id_artist") %>% 
    semi_join((df_node_artist_collection %>% filter(in_collection)), 
              by = c("id_artists_similar" = "id_artist")) 

  # Create similarity matrix between artists based on the distances
  qty_hops_max <- max(df_artists_similar$qty_hops)
  mat_distances <- df_artists_similar %>% 
    select(id_artist, id_artists_similar, qty_hops) %>% 
    pivot_wider(id_cols = id_artist, names_from = id_artists_similar, values_from = qty_hops,
                values_fill = qty_hops_max + 1) 

  dist_artists <- dist(mat_distances[, -1], method = "manhattan")
  mat_distance <- as.matrix(dist_artists) 
  
  mds_artists <- cmdscale(mat_distance)
  
  df_artist_mds <- tibble(
    id_artist = mat_distances$id_artist,
    x = mds_artists[, 1],
    y = mds_artists[, 2]
  ) 
  
  
  return(df_artist_mds)
}

get_graph_releases <- function() {
  
  lst_network <- get_release_network()
  
  graph_all <- graph_from_data_frame(lst_network$df_edges, 
                                     vertices = lst_network$df_nodes, 
                                     directed = FALSE)
  
  # Remove the artists from the network
  performers_with_release <- V(graph_all)$qty_releases > 0
  qty_node_edges <- degree(graph_all)
  multiple_edges <- V(graph_all)$name %in% names(qty_node_edges[qty_node_edges > 1])
  keep <- performers_with_release | multiple_edges
  remove <- !keep
  graph_reduced <- delete_vertices(graph_all, V(graph_all)[remove])
  
  # Remove mutually affirming edges
  graph_reduced <- simplify(graph_reduced)
  
  return(graph_reduced)
}
