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
