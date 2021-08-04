library(visNetwork)
library(igraph)
library(foreach)
library(doParallel)

calculate_artist_similarity <- function(df_artists, df_collection_items, 
                                        df_artist_members, df_artist_groups, df_artist_aliases,
                                        recalculate = FALSE){
  
  if(recalculate){
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
    
    df_nodes_artists %<>% 
      group_by(id_artist, name_artist) %>% 
      summarise(url_thumbnail = max(url_thumbnail),
                type_node = max(type_node)) %>% 
      ungroup()
    
    # Create the links between artists based of cooperation
    df_edges_artists <- bind_rows(
      df_members  %>% select(from = id_artist, to = id_member, type_edge),
      df_groups   %>% select(from = id_group, to = id_artist, type_edge),
      df_aliases  %>% select(from = id_artist, to = id_alias, type_edge)
    )  %>% unique()
    
    rm(df_members, df_groups, df_aliases)
    
    # Create a graph with the relationships between all artists (groups and aliases)
    graphs_artists <- graph_from_data_frame(df_edges_artists, vertices = df_nodes_artists, directed = FALSE)
    
    # Calculate shortest distances between artists
    
    # Get sub-graphs of interconnected artists
    lst_sub_graph <- decompose(graphs_artists)
    qty_artists_sub_graph <- lengths(lapply(lst_sub_graph, '[[') )
    
    # Remove unconnected artists (hoping to reduce calculation times)
    lst_sub_graph[qty_artists_sub_graph == 1] <- NULL
    qty_artists_sub_graph <- qty_artists_sub_graph[qty_artists_sub_graph != 1]
    
    pb_artists <- txtProgressBar(min = 0, max = sum(qty_artists_sub_graph), style = 3)
    lst_artist <- list()
    idx_artists <- 0
    for(sub_graph in lst_sub_graph){

      for(i in 1:length(V(sub_graph))){
        
        idx_artists <- idx_artists + 1
        setTxtProgressBar(pb_artists, value = idx_artists)
        lst_artist[[idx_artists]] <- tibble(
          id_artists_similar = V(sub_graph)$name,
          id_artist = rep(V(sub_graph)[[i]]$name, length(V(sub_graph))),
          qty_sim = all_shortest_paths(sub_graph, from = V(sub_graph)[[i]])$nrgeo
        )
      }       
    }

    df_artists_similar <- bind_rows(lst_artist) %>% 
      mutate(qty_sim = ifelse(id_artist == id_artists_similar, 0, qty_sim)) %>% 
      group_by(id_artist) %>% 
      mutate(perc_sim = qty_sim/sum(qty_sim),
             qty_artist = sum(qty_sim)) %>% 
      ungroup() 
    
    # Store distances in the database
    db_discogs <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))
    dbWriteTable(db_discogs, "artist_distances", df_artists_similar, overwrite = TRUE)
    
  } else {
    
    db_discogs <- dbConnect(RSQLite::SQLite(), paste0(config$db_location,"/discogs.sqlite"))
    df_artists_similar <- dbReadTable(db_discogs, "artist_distances")
  }

  # Create similarity matrix between artists based on the distances
  df_artists_similar %<>% 
    pivot_wider(id_cols = id_artist, names_from = id_artists_similar, values_from = perc_sim)

  # Get the relations between artists and collection items
  df_releases <- df_collection_items %>% 
    left_join(df_collection_artists, by = "id_release") %>% 
    select(id_artist) %>% 
    mutate(id_artist = paste0("a_", id_artist),
           qty_releases = 1)
  
  df_nodes_artists %<>% 
    left_join(df_releases, by = "id_artist") %>% 
    group_by(id_artist, name_artist) %>% 
    summarise(url_thumbnail = max(url_thumbnail),
              type_node     = max(type_node, na.rm = TRUE),
              qty_collection_items = sum(qty_releases, na.rm = TRUE)) %>% 
    ungroup()
  
  df_artists_similar %<>%
    left_join(df_nodes_artists, by = "id_artist")
  
  return(df_artists_similar)
}

