#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(magrittr)
library(igraph)
library(RSQLite)
library(yaml)
library(scales)
library(ggimage)
library(visNetwork)
source("get_network_data_functions.R")
source("cluster_navigation.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Background coloring
    setBackgroundColor(
        color = "black",
        gradient = c("linear", "radial"),
        direction = c("bottom", "top", "right", "left"),
        shinydashboard = FALSE
    ),

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            visNetworkOutput("network_artist_clusters", height = "800px", width = "100%")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    config <- read_yaml("config.yml")
    
    # Getting the whole graph and load clustering result
    graph_releases <- get_artist_clusters(file_db = paste0(config$db_location, "/discogs.sqlite"), 
                                          file_cluster_result = config$file_cluster_results,
                                          do_cluster_calculation = FALSE)
    clust_releases <- read_rds(config$file_cluster_results)
    
    # Get highest clustering
    lst_search_item <- list()
    iter_graph      <- 1
    lst_search_item[[iter_graph]] <- list()
    lst_search_item[[iter_graph]] <- get_cluster(graph_rel   = graph_releases,
                                                 res_clust   = clust_releases, 
                                                 search_item = lst_search_item[[iter_graph]])
    

    output$network_artist_clusters <- renderVisNetwork({
        
        nw_cluster <- cluster_to_network(lst_search_item[[iter_graph]]$graph)
        
        plot_network(nw_cluster) %>%
            visLayout(improvedLayout = TRUE) #>% 
            visEvents(#click = "function(nodes) { Shiny.onInputChange('artist_id_node', nodes.nodes); ;}",
                      doubleClick = "function(nodes) { Shiny.onInputChange('cluster_id_node', nodes.nodes); ;}")
    })
    
    observe({
        if(length(input$cluster_id_node) > 0){
            
            print(input$cluster_id_node)
            iter_graph <<- iter_graph + 1
            lst_search_item[[iter_graph]] <<- list(id_cluster_selected = input$cluster_id_node)
            lst_search_item[[iter_graph]] <<- get_cluster(graph_rel   = graph_releases,
                                                          res_clust   = clust_releases, 
                                                          search_item = lst_search_item[[iter_graph]],
                                                          search_item_previous = lst_search_item[[iter_graph-1]])
            
            visNetworkProxy("network_artist_clusters") #%>%
                # visRemoveNodes(nw_removed$df_nodes$id) %>% 
                # visRemoveEdges(nw_removed$df_edges$id) %>% 
                # visUpdateNodes(nw_artist_new$df_nodes) %>% 
                # visUpdateEdges(nw_artist_new$df_edges) %>%
                # visFit() 
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
