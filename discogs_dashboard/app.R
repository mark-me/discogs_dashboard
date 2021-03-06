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
            actionButton("btn_reset", "Reset"),
            actionButton("btn_back", "Back")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            visNetworkOutput("network_artist_clusters", height = "800px", width = "100%")
        )
    )
)

# Define server logic 
server <- function(input, output) {

    config <- read_yaml("config.yml")
    file_db <- paste0(config$db_location, "/discogs.sqlite")
    file_cluster_result <- paste0(config$db_location, "/", config$file_cluster_result)

    # Getting the whole graph and load clustering result
    lst_network <- list(
        nw_performer_releases = get_performer_master_network(file_db),
        res_clustering = read_rds(file_cluster_result)
    )
    
    # Search values
    lst_searches <<- list()
    search <- reactiveValues(
        idx = 1,
        id_cluster = NA
    )
    
    # Get highest clustering level
    lst_searches[[1]] <- get_clustered_network(lst_network, 
                                               lst_search_results  = NA, 
                                               id_cluster_selected = NA)
    nw_cluster <- lst_searches[[1]]$nw_cluster
    df_performers <- get_cluster_performers(df_network_nodes = lst_network$nw_performer_releases$df_nodes,
                                            df_cluster_ids   = lst_searches[[1]]$df_cluster_ids)
    df_releases   <- get_cluster_releases(df_network_nodes = lst_network$nw_performer_releases$df_nodes,
                                          df_cluster_ids   = lst_searches[[1]]$df_cluster_ids)
    
    output$network_artist_clusters <- renderVisNetwork({
        
        search$id_cluster
        plot_network(nw_cluster) %>%
            visLayout(improvedLayout = TRUE) %>% 
            visEvents(#click = "function(nodes) { Shiny.onInputChange('artist_id_node', nodes.nodes); ;}",
                      doubleClick = "function(nodes) { Shiny.onInputChange('id_cluster', nodes.nodes); ;}")
    })
    
    get_next_cluster <- function(idx, id_cluster){
        
       print(paste0("Iteration: ", idx, "/ id_cluster: ", id_cluster))
       lst_searches[[idx]] <<- get_clustered_network(lst_network,
                                                     lst_search_results  = ifelse(is.na(id_cluster), NA, lst_searches),
                                                     id_cluster_selected = id_cluster)
       nw_cluster <<- lst_searches[[idx]]$nw_cluster
       
       df_performers <<- get_cluster_performers(df_network_nodes = lst_network$nw_performer_releases$df_nodes,
                                                df_cluster_ids   = lst_searches[[idx]]$df_cluster_ids)
       df_releases   <<- get_cluster_releases(df_network_nodes = lst_network$nw_performer_releases$df_nodes,
                                              df_cluster_ids   = lst_searches[[idx]]$df_cluster_ids)
    }
    
    observeEvent(input$id_cluster,{
        if(length(input$id_cluster) > 0){
            search$idx        <- search$idx + 1
            search$id_cluster <- input$id_cluster
            get_next_cluster(search$idx, search$id_cluster)
        }
    })
    
    observeEvent(input$btn_reset,{
        search$idx        <- 1
        search$id_cluster <- NA
        get_next_cluster(search$idx, search$id_cluster)
    })
 
 
}

# Run the application 
shinyApp(ui = ui, server = server)
