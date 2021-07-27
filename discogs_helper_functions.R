discogs_token <- function() {
  val <- Sys.getenv("DISCOGS_TOKEN")
  if (identical(val, "")) {
    stop("`DISCOGS_TOKEN` env var has not been set")
  }
  val
}

discogs_user <- function() {
  val <- Sys.getenv("DISCOGS_USER")
  if (identical(val, "")) {
    stop("`DISCOGS_USER` env var has not been set")
  }
  val
}

api_discogs_config <- list(
  url = "https://api.discogs.com",
  username = discogs_user(),
  token = discogs_token ()
)

rm(discogs_token, discogs_user)

extract_sublists_as_df <- function(df, colname_id, colname_list){
  
  lst_dfs <- list()
  for(row in 1:nrow(df)){
    
    lst <- df[row, colname_list][[1]][[1]]
    lst_dfs[[row]] <- cbind(
      id_release = unlist(df[row, colname_id], use.names = FALSE),
      bind_rows(lst)
    )  
  }
  
  df <- bind_rows(lst_dfs)
  
  return(df)
}

extract_vectors_as_df <- function(df, colname_id, colname_vec, colname_dest){
  
  lst_df <- list()
  
  for(row in 1:nrow(df)){
    
    vec_values <- df[row, colname_vec][[1]][[1]]
    vec_id     <- rep(df[row, colname_id][[1]][[1]], length(vec_values))
    
    df_split <- as_tibble( cbind(
      vec_id,
      vec_values ) )
    
    names(df_split) <- c(colname_id, colname_dest)
    if(nrow(df_split) > 0){
      lst_df[[row]] <- df_split  
    }
  }
  
  df_result <- bind_rows(lst_df)
  
  return(df_result)  
}

