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