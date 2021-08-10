# Discogs Dashboard

Creating a buyer dashboard based on their [Discogs](https://www.discogs.com) collection, which allows users to browse through their collection and get suggestions for buying based on that collection.

# Storing credentials

I've used the _.Renviron_ file to store my Discogs token and user name so those aren't in my public code. The steps to set these yourself:
```
usethis::edit_r_environ()
```
Add the lines
```
DISCOGS_TOKEN=<your-token>
DISCOGS_USER=<your-user-name>
```
And restart the R session.

# Me just plowing through

Rules for likeness
* A performer node with lots of performer connections should have performer to performer edges with lower weight
* An edge between a node with high number of releases and a performer with

Data structure
* Change data loading for releases and collection items do the key column names match (id_release_main is *not* a master)