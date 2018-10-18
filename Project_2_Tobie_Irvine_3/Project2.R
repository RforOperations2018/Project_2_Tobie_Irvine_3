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
 sidebar <- dashboardSidebar(
   sidebarMenu(
     id = "tabs",
     menuItem("Charts", icon = icon("bar-chart"), tabName = "chart"),
     menuItem("Data Table", icon = icon("table"), tabName = "datatable"),
     menuItem("Map", icon = icon("map"), tabName = "map"),
     radioButtons(),
     selectInput(),
     selectInput(),
     sliderInput()
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
 server <- function(input, output, session) {
   #Reactive Element for Charts and Datatable 
   df.filter <- reactive ({
     url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%227f5da957-01b5-4187-a842-3f215d30d7e8%22%20WHERE%20%22Gender%22%20=%20%27", 
                   input$gender,"%27%20AND%20%22Race%22%20%3D%20%27", input$race, "%27%20")
     # use ckan on url and make clean data
     clean.data <- ckanSQL(url) %>% na.omit() %>%
       mutate(Gender = plyr::revalue(Gender,  c("F" = 'Female',  "M" = 'Male'))
       ) %>%
       mutate(Race = plyr::revalue(Race,  c("A" = 'Asian',  "B" = 'Black', "H" = 'Hispanic', "I" = 'Indian', "U" = 'Unknown', "W" = 'White', "x" = 'Unreported')))
     print(colnames(clean.data))
     return(clean.data)
   })
   
   #Reactive Element for Map
   df.filter2 <- reactive ({
     url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22 7f5da957-01b5-4187-a842-3f215d30d7e8 %22%20WHERE%20%22Gender%22%20=%20%27", 
                   input$gender,"%27%20AND%20%22Race%22%20%3D%20%27", input$race, "%27%20")
     # use ckan on url and make clean data
     clean.data <- ckanmap(url) 
     return(clean.data)
   })
  
   
   #Create Chart 1  
   output$plot1 <- renderPlot({
     df2 <- deathInput()
     ggplot(df2, aes(x = State, y = Deaths, color = State)) + 
       geom_bar(stat = "identity") + 
       ggtitle("Total Deaths per Accident per Year") + 
       ylab("Total Deaths") +
       theme(axis.text.x = element_text(angle = 90, hjust = 1))
   })
   
   #Create Chart 2
   output$plot2 <-  renderPlot({
     df3 <- deathInput()
     ggplot(df3, aes(x = State, y = Age.adjusted.Death.Rate, color = State)) + 
       geom_bar(stat = "identity") + 
       ggtitle("Death Rate (per 100,000) per Accident per Year") + 
       ylab("Adjusted Death Rate")
   })
   #Create Interactive Map
   output$mymap <- render$leaflet({
     
   trial <- readOGR("2010_Census_Tracts.geojson")
   
   #Create map
   leaflet(data = trial) %>%
     addProviderTiles("OpenMapSurfer.Roads", options = providerTileOptions(noWrap = TRUE), group = "Default") %>%
     addProviderTiles("Esri.DeLorme", options = providerTileOptions(noWrap = TRUE), group = "Topographical") %>%
     addProviderTiles(providers$Esri.WorldImagery, options = providerTileOptions(noWrap = TRUE), group = "World") %>%
     # Layers Control
     addLayersControl(
       baseGroups = c("Default", "Topographical", "World"),
       options = layersControlOptions(collapsed = FALSE)
     )  %>% addPolylines(color = "#63CBD4", popup = ~LINENAME)
   })
   # Create a Data Table
   output$datatable <- DT::renderDataTable({
     subset(deathInput(), select = c(State, Cause.Name, Deaths, Age.adjusted.Death.Rate))
   })
 }
 
 # Run the application 
 shinyApp(ui = ui, server = server)

