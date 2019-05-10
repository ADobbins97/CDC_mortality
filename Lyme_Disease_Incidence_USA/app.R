#    http://shiny.rstudio.com/
#
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
library(tidyverse)
library(plotly)

read_rds("cdc_worked_data.rds")

# Define UI for application that draws a histogram
ui <- navbarPage(
  "Lyme Disease: the United States",
  theme = shinytheme("cosmo"),

  tabPanel(
    "Home",
    fluidPage(
      fluidRow(
        h1("About This Project"),
        h3("Lyme Disease: Statistics and History in the United States"),
        p("Every year, health care providers around the country report likely cases to the CDC. They, in turn assimilate this information into reports and datasets which are available to the public. However, because states and organizations report independently, the types of data is not always consistent and therefore in this study, only New York will be analyzed on a state-level for the sake of continuity."),
        br(),
        p("Every year, up to 300,000 cases of Lyme disease are reported. The disease is caused by a bacterium called Borrelia burgdoferi. It is passed to humans by ticks, although not all types of ticks carry it. After being bitted a `bulls-eye` circle appears around the bite site and this is the first warning sign you might have contracted Lyme. The problem is that sometimes people do not find the bite, and think they have a cold -- fever, headache, fatigue."),
        br(),
        p("However, symptoms quickly progress beyond the flu-like symptoms and the disease can be fully debilitating. Untreated, it will spread to one's joints, heart, and nervous system and cause a multitude of side effects: 
          Extreme fatigue, memory loss, mood swings, depression, anxiety, headaches, muscle fatigue, bone pain, abdominal disruption, bloating, digestive issues, loss of appetite, facial swelling, thyroid disease, and many more.
          It can often be difficult to identify someone has lyme in the absence of visible physical markers. Thus, the data here is only the identified likely cases which are reported to the CDC.")
      )
    )
  ),

  tabPanel(
    "View Interactive Diagnoses",
    fluidPage(
      fluidRow(
        titlePanel("Tick Collection in New York from 2008 to 2017"),
        br(),
        # Sidebar with a slider input for number of bins
        sidebarLayout(
          sidebarPanel(
            mainPanel(plotOutput("NYPlot", width = 500, height = 500))
          ),
          position = "left",
          pickerInput("year", "Year", choices = c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"), selected = c("2000"), options = list(`actions-box` = TRUE), multiple = FALSE)
          # Show a plot of the generated distribution
        )
      )
      )
    ),

      tabPanel(
        "View Interactive Diagnoses",
        fluidPage(
          fluidRow(
            h1("Reported Cases of Lyme Disease per State, from 2000 to 2017"),
            br(),
            sidebarLayout(
              sidebarPanel(
                mainPanel(plotOutput("USMap_year", width = 500, height = 500))),
                
              position = "left",
              pickerInput("year", "Year", choices = c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"), selected = c("2000"), options = list(`actions-box` = TRUE), multiple = FALSE)
              )))),
              # Show a plot of the generated distribution
              
              tabPanel(
                "View Interactive Diagnoses",
                fluidPage(
                  fluidRow(
                    h1("Reported Cases of Lyme Disease per State, from 2000 to 2017"),
                    br(),
                    sidebarLayout(
              sidebarPanel(
                mainPanel(plotOutput("USMap", width = 500, height = 500))),
          
            position = "left",
            pickerInput("year", "Year", choices = c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"), selected = c("2000"), options = list(`actions-box` = TRUE), multiple = FALSE)
            # Show a plot of the generated distribution
          )
        )
      )
    ),

      tabPanel(
        "Age of Diagnoses",
        fluidPage(
          fluidRow(
            titlePanel("Cases of Lyme by Age from 2010 to 2017"),
            br(),
            # Sidebar with a slider input for number of bins
            sidebarLayout(
              sidebarPanel(
                mainPanel(plotOutput("Age", width = 500, height = 500))
              ),
              position = "left",
              pickerInput("year", "Year", choices = c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"), selected = c("2017"), options = list(`actions-box` = TRUE), multiple = FALSE)
              # Show a plot of the generated distribution
            )
          )
        )
      )
    )
  






# Define server logic required to draw a histogram
server <- function(input, output) {
  output$USMap <- renderPlot({
    x1 <- urbnmapr::states
    y1 <- state_cases
    left_join(x1, y1, by = "state_name") %>%
      ggplot() +
      geom_polygon(
        mapping = aes(x = long, y = lat, group = group, fill = cases),
        color = "white"
      ) +
      geom_text(
        data = get_urbn_labels(map = "states"), aes(x = long, lat, label = state_abbv),
        size = 2
      ) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45)
  })

  output$USMap_year <- renderPlot({
    x1 <- urbnmapr::states
    y1 <- state_cases %>% filter(year == "input$year")
    left_join(x1, y1, by = "state_name") %>%
      ggplot() +
      geom_polygon(
        mapping = aes(x = long, y = lat, group = group, fill = cases),
        color = "white"
      ) +
      geom_text(
        data = get_urbn_labels(map = "states"), aes(x = long, lat, label = state_abbv),
        size = 2
      ) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45)
  })

  output$NYPlot <- renderPlotly({
    # need to layer this so outline of new york doesn't disappear
    x <- urbn_data
    y <- nymph_tick_number %>% filter(year == input$year) %>% select(county_name, total_ticks)
    ny_plot <-
      left_join(x, y, by = "county_name") %>%
      ggplot(mapping = aes(long, lat, group = group, fill = total_ticks)) +
      geom_polygon(color = "white", size = .25) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      theme(
        legend.title = element_text(),
        legend.key.width = unit(.5, "in")
      ) +
      labs(fill = "Ticks Collected")
    
    ggplotly(p = ny_plot, width = 500, height = 500)
  })

  output$Age <- renderPlotly({
    year_data %>%
      select(year, age, cases) %>% 
      filter(year == "input$year") %>%
      ggplot(mapping = aes(x = age, y = cases)) +
      geom_col()
  })
}

# Run the application
shinyApp(ui = ui, server = server)