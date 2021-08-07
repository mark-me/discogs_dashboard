library(visNetwork)
library(igraph)
library(foreach)
library(doParallel)
source("data-prep/load_discogs_all.R")

calculate_artist_similarity <- function(df_artists, df_collection_items, 
                                        df_artist_members, df_artist_groups, df_artist_aliases,
                                        recalculate = FALSE){
  
  # Determine which members are/were part of a group
  df_members <- df_artists %>% 
    select(id_artist, name_artist) %>% 
    inner_join(df_artist_members, by = "id_artist") %>% 
    select(id_artist, name_artist, id_member, name_member, url_thumbnail) %>% 
    mutate(id_artist = paste0("a_", id_artist),
           id_member = paste0("a_", id_member),
           type_edge = "group_member",
           type_node = "group")
  
  # Determine the members of a group
  df_groups <- df_artists %>% 
    select(id_artist, name_artist) %>% 
    inner_join(df_artist_groups, by = "id_artist") %>% 
    select(id_artist, name_artist, id_group, name_group, url_thumbnail) %>% 
    mutate(id_artist = paste0("a_", id_artist),
           id_group = paste0("a_", id_group),
           type_edge = "group_member",
           type_node = "artist")
  
  # Determine aliases for artists
  df_aliases <- df_artists %>% 
    select(id_artist, name_artist) %>% 
    inner_join(df_artist_aliases, by = "id_artist") %>% 
    select(id_artist, name_artist, id_alias, name_alias, url_thumbnail) %>% 
    mutate(id_artist = paste0("a_", id_artist),
           id_alias  = paste0("a_", id_alias),
           type_edge = "alias",
           type_node = "alias")
  
  # Create the nodes of artist cooperations network
  df_nodes_artists <- bind_rows(
    df_artists %>% 
      select(id_artist, name_artist, url_thumbnail) %>% 
      mutate(type_node = "artist",
             id_artist = paste0("a_", id_artist)),
    df_members %>% 
      select(id_artist = id_member, name_artist = name_member, url_thumbnail, type_node),
    df_groups  %>% 
      select(id_artist = id_group,  name_artist = name_group, url_thumbnail, type_node),
    df_aliases %>% 
      select(id_artist = id_alias,  name_artist = name_alias, url_thumbnail, type_node),
  )
  
  # Make sure the nodes are unique
  df_nodes_artists %<>% 
    group_by(id_artist, name_artist) %>% 
    summarise(url_thumbnail = max(url_thumbnail),
              type_node = max(type_node)) %>% 
    ungroup()
  
  # Adding indicator whether there is a release of the artist in the user's collection
  # Added to speed up the distance calculation process for calculating distances only between
  # the artists which are in the user's collection
  df_node_artist_collection <- df_nodes_artists %>% 
    semi_join((df_collection_artists %>% mutate(id_artist = paste0("a_", id_artist))), 
              by = "id_artist") %>% 
    select(id_artist) %>% 
    mutate(in_collection = TRUE)
  
  df_nodes_artists %<>% 
    left_join(df_node_artist_collection, by = "id_artist") %<>% 
    mutate(in_collection = ifelse(is.na(in_collection), FALSE, in_collection))
  
  # Create the links between artists based of cooperation
  df_edges_artists <- bind_rows(
    df_members  %>% select(from = id_artist, to = id_member, type_edge),
    df_groups   %>% select(from = id_group, to = id_artist, type_edge),
    df_aliases  %>% select(from = id_artist, to = id_alias, type_edge)
  )  %>% unique()
  
  rm(df_members, df_groups, df_aliases)
  
  if(recalculate){
      # Create a graph with the relationships between all artists (groups and aliases)
    graphs_artists <- graph_from_data_frame(df_edges_artists, vertices = df_nodes_artists, directed = FALSE)
    
    # Calculate shortest distances between artists
    
    # Get sub-graphs of interconnected artists
    lst_sub_graph <- decompose(graphs_artists)
    qty_artists_sub_graph <- lengths(lapply(lst_sub_graph, '[[') )
    
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

