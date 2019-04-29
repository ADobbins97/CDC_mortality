#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(ggplot2)
library(urbnmapr)
library(urbnthemes)
library(readr)
library(janitor)
library(tm)
library(sf)
library(leaflet)
library(mapview)
library(tigris)
library(lubridate)
library(shiny)
library(shinyWidgets)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- 
  
  navbarPage("LYME DISEASE IN THE UNITED STATES",
             
             theme = shinytheme("cosmo"), 
             
             tabPanel("Main",
    fluidPage(
      fluidRow(
        h1("Hello!"),
        h3("Lyme Disease: About and History in the United States")
      )
    )
             ),
    
    tabPanel("State: New York"),
      fluidRow(
        fluidPage(
          titlePanel("Tick Collection in New York from 2008 to 2017"),
          br(),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         pickerInput("year",
                     "Year",
                     choices = c("2000",
                                 "2001",
                                 "2002",
                                 "2003",
                                 "2004",
                                 "2005",
                                 "2006",
                                 "2007",
                                 "2008",
                                 "2009",
                                 "2010",
                                 "2011",
                                 "2012",
                                 "2013",
                                 "2014",
                                 "2015",
                                 "2016",
                                 "2017"),
                     selected = c("2000"),
                     options = list(`actions-box` = TRUE),
                     multiple = FALSE)
      ),
    br(),
   
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("NYPlot")
        )
     ))
  ),
  
  tabPanel("National"),
    fluidRow(
      fluidPage(
        h1("Reported Cases of Lyme Disease per State, from 2000 to 2017"),
        br(),

   sidebarLayout(
     sidebarPanel(
       pickerInput("year",
                   "Year",
                   choices = c("2000",
                               "2001",
                               "2002",
                               "2003",
                               "2004",
                               "2005",
                               "2006",
                               "2007",
                               "2008",
                               "2009",
                               "2010",
                               "2011",
                               "2012",
                               "2013",
                               "2014",
                               "2015",
                               "2016",
                               "2017"),
                   selected = c("2000"),
                   options = list(`actions-box` = TRUE),
                   multiple = FALSE)
     )),
   br(),
   
   # Sidebar with a slider input for number of bins 
   leaflet( 
     addTiles( 
      addMarkers(stname = "WA", 
                 label = "40"),
                labelOptions = labelOptions(noHide = F)
  #addMarker2
  
     )),
     # Show a plot of the generated distribution
     mainPanel(
       plotOutput("USMap")
  ))
))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$NYPlot <- renderPlot({
     
     nymph_tick_number %>% 
       left_join(urbn_data, by = "county_name") %>%
       ggplot(mapping = aes(long, lat, group = group)) +
       geom_polygon(color = "white", size = .25) +
       coord_map(projection = "albers", lat0 = 39, lat1 = 45) 
#need to layer this so outline of new york doesn't disappear     
     nymph_tick_number %>% 
       left_join(urbn_data, by = "county_name") %>% 
       filter(year == input$year) %>% 
     ggplot(mapping = aes(long, lat, group = group, fill = total_ticks)) +
       geom_polygon(color = "white", size = .25) +
       coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
       theme(legend.title = element_text(),
             legend.key.width = unit(.5, "in")) +
       labs(title = "",
            fill = "Ticks Collected")
   })
   
   output$USMap <- renderPlot({
     
     state_cases %>%    
       ggplot() + 
       geom_polygon(data = urbnmapr::states, mapping = aes(x = long, y = lat, group = group),
                    fill = "#a2d4ec", color = "white") +
       geom_text(data = get_urbn_labels(map = "states"), aes(x = long, lat, label = state_abbv), 
                 size = 2) +
       coord_map(projection = "albers", lat0 = 39, lat1 = 45)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

