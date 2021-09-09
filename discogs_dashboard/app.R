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
    file_db <- paste0(config$db_location, "/discogs.sqlite")
    file_cluster_result <- paste0(config$db_location, "/", config$file_cluster_result)

    # Getting the whole graph and load clustering result
    lst_network <- list(
        res_clustering = read_rds(file_cluster_result),
        nw_performer_releases = get_performer_master_network(file_db)
    )
    
    # Get highest clustering
    lst_search_results <- list()
    iter_graph <- 1
    lst_search_results[[iter_graph]] <- get_clustered_network(lst_network, lst_search_results = NA, id_cluster_selected = NA)
    

    output$network_artist_clusters <- renderVisNetwork({
        
        nw_cluster <- lst_search_results[[iter_graph]]$nw_cluster
        nw_cluster$df_nodes %<>% 
            mutate(label = paste0(id_cluster, "-", name_authoritative))
        
        plot_network(nw_cluster) %>%
            visLayout(improvedLayout = TRUE) %>% 
            visEvents(#click = "function(nodes) { Shiny.onInputChange('artist_id_node', nodes.nodes); ;}",
                      doubleClick = "function(nodes) { Shiny.onInputChange('cluster_id_node', nodes.nodes); ;}")
    })
    
    observe({
        if(length(input$cluster_id_node) > 0){
            
            print(input$cluster_id_node)
            iter_graph <<- iter_graph + 1
            lst_search_results[[iter_graph]] <<- get_clustered_network(lst_network, 
                                                                       lst_search_results, 
                                                                       id_cluster_selected = cluster_id_node)

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
