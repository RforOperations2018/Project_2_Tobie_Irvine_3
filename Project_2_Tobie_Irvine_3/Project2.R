library(plyr)
library(dplyr)
library(plotly)
library(ggplot2)
library(shiny)
library(reshape2)
library(data.table)
library(DT)
library(httr)
library(jsonlite)
library(plotly)
library(htmltools)
library(leaflet)
library(rgdal)
library(leaflet.extras)
library(shinydashboard)


## Creation of ckan function for csv data (see Homework 4)
ckanSQL <- function(url) {
  # Make the Request
  r <- RETRY("GET", URLencode(url))
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  json <- gsub('NaN', 'NA', c, perl = TRUE)
  # Create Dataframe
  data.frame(jsonlite::fromJSON(json)$result$records)
}

## Creation of ckan function for geojson data
ckanMap <- function(url) {
  # Make the Request
  r <- RETRY("GET", URLencode(url))
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  json <- gsub('NaN', 'NA', c, perl = TRUE)
  # Create Dataframe
  readOGR(json)
}
## Unique values for Resource Field
ckanUnique <- function(id, field) {
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}

##Create choices for fields of Race and Gender
# gender_choices <- sort(ckanUnique("7f5da957-01b5-4187-a842-3f215d30d7e8", "Gender")$Gender)
# race_choices <- sort(ckanUnique("7f5da957-01b5-4187-a842-3f215d30d7e8", "Race")$Race)

#Relabel choices of gender and race (input in user interface)
# Moved gender choice refactoring to the reactive function


##UI for Shiny App
header <- dashboardHeader(title = "hahahaha")
 sidebar <- dashboardSidebar(
  
   sidebarMenu(
     id = "tabs",
     menuItem("Charts", icon = icon("bar-chart"), tabName = "chart"),
     menuItem("Data Table", icon = icon("table"), tabName = "datatable"),
     menuItem("Map", icon = icon("map"), tabName = "map"),
     selectInput(inputId = "neighborhood", label = "Pick a neighborhood", choices = c("Bloomington", "Problem"))
   )
 )
 
 body <- dashboardBody(
   tabItems(
     tabItem("plot",
           fluidRow(
               tabBox(title = "Plot",
                      width = 12,
                      tabPanel("Chart 1", plotOutput("plot1")),
                      tabPanel("Chart 2", plotOutput("plot2"))
                      )
             )
     ),
     tabItem("datatable",
             fluidPage(
               box(title = "Selected Character Stats", DT::dataTableOutput("datatable"), width = 12))
     ),
     tabItem("map",
             fluidPage(
               box(title = "Map",leafletOutput("mymap") )
                       )
             
   ) 
 ))
 # Define UI for shiny dashboard
 ui <- dashboardPage(header, sidebar, body)
 
 # Define server logic
 server <- function(input, output) {
  
   #Reactive Element for Map
   df.filter2 <- reactive ({
     url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%228d76ac6b-5ae8-4428-82a4-043130d17b02%22%20WHERE%20%22neighborhood%22%20=%20%27Bloomfield%27%20")
     # use ckan on url and make clean data
     clean.data <- ckanSQL(url)
     print(colnames(clean.data))
     return(clean.data)
   })
   #Create Interactive Map
   output$mymap <- renderLeaflet({
     
   trial <- readOGR("2010_Census_Tracts.geojson") %>% na.omit
   fires <- df.filter2()
   #Create map
   leaflet() %>%
     addProviderTiles("OpenStreetMap.France", options = providerTileOptions(noWrap = TRUE), group = "Default") %>%
     # addProviderTiles("Esri.DeLorme", options = providerTileOptions(noWrap = TRUE), group = "Topographical") %>%
     # addProviderTiles(providers$Esri.WorldImagery, options = providerTileOptions(noWrap = TRUE), group = "World") %>%
     # # Layers Control
     # addLayersControl(
     #   baseGroups = c("Default", "Topographical", "World"),
     #   options = layersControlOptions(collapsed = FALSE)
     # )  %>%
     #addPolygons(data = trial) %>%
     addCircleMarkers(data = fires, lng = ~longitude, lat = ~latitude, radius = 1.5)
   })
   
   #Reactive Element for Charts and Datatable 
   # df.filter <- reactive ({
   #   url <- paste0()
   #   # use ckan on url and make clean data
   #   clean.data <- ckanSQL(url) %>% na.omit()
   #    
   #   print(colnames(clean.data))
   #   return(clean.data)
   # })
   # Create a Data Table
   # output$datatable <- DT::renderDataTable({
   #   subset(deathInput(), select = c(State, Cause.Name, Deaths, Age.adjusted.Death.Rate))
   # })
   
   #Create Chart 1  
   # output$plot1 <- renderPlot({
   #   df2 <- deathInput()
   #   ggplot(df2, aes(x = State, y = Deaths, color = State)) + 
   #     geom_bar(stat = "identity") + 
   #     ggtitle("Total Deaths per Accident per Year") + 
   #     ylab("Total Deaths") +
   #     theme(axis.text.x = element_text(angle = 90, hjust = 1))
   # })
   # 
   # #Create Chart 2
   # output$plot2 <-  renderPlot({
   #   df3 <- deathInput()
   #   ggplot(df3, aes(x = State, y = Age.adjusted.Death.Rate, color = State)) + 
   #     geom_bar(stat = "identity") + 
   #     ggtitle("Death Rate (per 100,000) per Accident per Year") + 
   #     ylab("Adjusted Death Rate")
   # })
 }
 
 # Run the application 
 shinyApp(ui = ui, server = server)

