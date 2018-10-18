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

## Creation of ckan function
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
# Unique values for Resource Field
ckanUnique <- function(id, field) {
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}
#Create choices for fields of Race and Gender
# gender_choices <- sort(ckanUnique("7f5da957-01b5-4187-a842-3f215d30d7e8", "Gender")$Gender)
# race_choices <- sort(ckanUnique("7f5da957-01b5-4187-a842-3f215d30d7e8", "Race")$Race)

#Relabel choices of gender and race (input in user interface)
# Moved gender choice refactoring to the reactive function

 
 sidebar <- dashboardSidebar(
   sidebarMenu(
     id = "tabs",
     menuItem("Charts", icon = icon("bar-chart"), tabName = "plot"),
     menuItem("Death Data Table", icon = icon("table"), tabName = "datatable"),
     radioButtons("cause",
                  "Pick a cause of death: ",
                  choices = unique(State.Death.data$Cause.Name),
                  #multiple = FALSE,
                  #selectize = TRUE,
                  selected = "Alzheimer's disease"
     ),
     selectInput("state",
                 "State:",
                 choices = sort(unique(State.Death.data$State)),
                 multiple = TRUE,
                 selectize = TRUE,
                 selected = "Alabama"),
     selectInput("year", 
                 "Pick a Year: ",
                 choices = sort(unique(State.Death.data$Year)),
                 multiple = FALSE,
                 selectize = TRUE,
                 selected = 1999
     )
   )
 )
 
 body <- dashboardBody(
   tabItems(
     tabItem("plot",
             # Don't be afraid to delete stuff. Once you commit it you can always go back and grab it from Git History!
             # fluidRow(
             #   infoBoxOutput("mass"),
             #   valueBoxOutput("height")
             # ),
             fluidRow(
               tabBox(title = "Plot",
                      width = 12,
                      tabPanel("Cumulative Deaths", plotOutput("plot1")),
                      tabPanel("Death Rate", plotOutput("plot2")),
                      tabPanel("Time Plot", plotOutput("plot3")))
             )
     ),
     tabItem("datatable",
             fluidPage(
               box(title = "Selected Character Stats", DT::dataTableOutput("datatable"), width = 12))
     )
   ) 
 )
 
 
 # Define UI for shiny dashboard
 ui <- dashboardPage(header, sidebar, body)
 
 # Define server logic
 server <- function(input, output) {
   #Reactive Elements 
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
   output$map <-  renderPlot({
     df4 <- 
     
   })
   # Create a Data Table
   output$datatable <- DT::renderDataTable({
     subset(deathInput(), select = c(State, Cause.Name, Deaths, Age.adjusted.Death.Rate))
   })
 }
 
 # Run the application 
 shinyApp(ui = ui, server = server)

