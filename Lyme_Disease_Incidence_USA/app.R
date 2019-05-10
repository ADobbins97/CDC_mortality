library(urbnmapr)
library(plotly)
library(base)
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

urbn_data <-
  urbnmapr::counties %>%
  filter(state_abbv == "NY") %>%
  mutate(county_name = removeWords(county_name, " County")) %>%
  group_by(county_name)

rds_data <- read_rds("cdc_worked_data.rds")

# Define UI for application that draws a histogram
ui <- navbarPage(
  "Lyme Disease: the United States",
  theme = shinytheme("cosmo"),

  tabPanel(
    "Home",
    fluidRow(
      mainPanel(
        h1("About This Project"),
        h3("Lyme Disease: Statistics and History in the United States"),
        p("Every year, health care providers around the country report likely cases to the CDC. They, in turn assimilate this information into reports and datasets which are available to the public. However, because states and organizations report independently, the types of data is not always consistent and therefore in this study, only New York will be analyzed on a state-level for the sake of continuity."),
        br(),
        p("Every year, up to 300,000 cases of Lyme disease are reported. The disease is caused by a bacterium called Borrelia burgdoferi. It is passed to humans by ticks, although not all types of ticks carry it. After being bitted a `bulls-eye` circle appears around the bite site and this is the first warning sign you might have contracted Lyme. The problem is that sometimes people do not find the bite, and think they have a cold -- fever, headache, fatigue."),
        br(),
        p("However, symptoms quickly progress beyond the flu-like symptoms and the disease can be fully debilitating. Untreated, it will spread to one's joints, heart, and nervous system and cause a multitude of side effects: 
          Extreme fatigue, memory loss, mood swings, depression, anxiety, headaches, muscle fatigue, bone pain, abdominal disruption, bloating, digestive issues, loss of appetite, facial swelling, thyroid disease, and many more.
          It can often be difficult to identify someone has lyme in the absence of visible physical markers. Thus, the data here is only the identified likely cases which are reported to the CDC.")
      ),
      sidebarPanel(
        h3("About the Author."),
        helpText("Alexandra Dobbins is a junior at Harvard College. This ShinyApp is ")
      )
    )
  ),

  tabPanel(
    "New York Tick Samples",
    fluidPage(
      fluidRow(
        titlePanel("Nymph Tick Collection in New York"),
        br(),
        sidebarPanel(
          h1("Tick-borne Bacteria"),
          h3("Info about bacteria in these maps")
        ),
        # box(
        mainPanel(plotOutput("NYPlot_Nymph", width = 500, height = 500)),
        position = "left",
        selectInput("year", label = "Year", choices = list(
          "2008", "2009", "2010", "2011", "2012", "2013", "2014",
          "2015", "2016", "2017"
        ), selected = "2000", multiple = FALSE)
        # )
      ),

      fluidRow(
        # box(
        titlePanel("Adult Tick Collection in New York"),
        br(),
        sidebarLayout(
          mainPanel(plotOutput("NYPlot_Adult", width = 500, height = 500)),
          position = "left",
          selectInput("year", "Year", choices = c(
            "2008", "2009", "2010", "2011", "2012", "2013", "2014",
            "2015", "2016", "2017"
          ), selected = c("2000"), multiple = FALSE)
        )
      )
      # )
    )
  ),

  tabPanel(
    "Annual Diagnoses",
    fluidPage(
      fluidRow(
        h1("Reported Cases of Lyme Disease per State, from 2000 to 2017"),
        br(),
        sidebarLayout(
          mainPanel(plotOutput("USMap_year", width = 500, height = 500)),
          position = "left",
          selectInput("year", label = "Year", choices = list(
            "2008", "2009", "2010", "2011", "2012", "2013", "2014",
            "2015", "2016", "2017"
          ), selected = "2000")
        )
      )
    )
  ),
  # Show a plot of the generated distribution

  tabPanel(
    "View Interactive Diagnoses",
    fluidPage(
      fluidRow(
        h1("Reported Cases of Lyme Disease per State, from 2000 to 2017"),
        br(),
        sidebarLayout(
          mainPanel(plotOutput("USMap", width = 500, height = 800)),
          position = "left",
          selectInput("year", label = "Year", choices = list(
            "2008", "2009", "2010", "2011", "2012", "2013", "2014",
            "2015", "2016", "2017"
          ), selected = "2000")
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
          mainPanel(plotOutput("Age", width = 500, height = 500)),
          position = "left",
          selectInput("year", label = "Year", choices = list(
            "2008", "2009", "2010", "2011", "2012", "2013", "2014",
            "2015", "2016", "2017"
          ), selected = "2000")
          # Show a plot of the generated distribution
        )
      )
    )
  )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$USMap <- renderPlot({
    shapes %>% filter(year == "input$year")
    
    ggplot(data = shapes) +
      geom_sf(mapping = aes(fill = cases))+
      xlab("Longitude") + ylab("Latitude") +
      labs(title = "Reported Cases of Lyme Disease in the United States", subtitle = "Cases Reported per State, Sorted", fill = "Cases") +
      theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                            size = 0.5), panel.background = element_rect(fill = "aliceblue"))
  })

  output$USMap_year <- renderPlot({
    x1 <- urbnmapr::states
    y1 <- rds_data %>% state_cases %>% filter(year == "input$year")
    usmap_year_data <-
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

  output$NYPlot_Nymph <- renderPlot({
    x <- rds_data %>% urbn_data
    y <- rds_data %>% nymph_tick_number %>% filter(year == "input$year")

    ny_plot_nymph <-
      left_join(x, y, by = "county_name") %>%
      ggplot(mapping = aes(long, lat, group = group, fill = total_ticks)) +
      geom_polygon(color = "white", size = .25) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      theme(
        legend.title = element_text(),
        legend.key.width = unit(.5, "in")
      ) +
      labs(fill = "Ticks Collected")
  })

  output$NYPlot_Adult <- renderPlot({
    x <- rds_data %>% urbn_data
    y <- rds_data %>% adult_tick_number %>% filter(year == "input$year")

    ny_plot_adult <-
      left_join(x, y, by = "county_name") %>%
      ggplot(mapping = aes(long, lat, group = group, fill = total_ticks)) +
      geom_polygon(color = "white", size = .25) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      theme(
        legend.title = element_text(),
        legend.key.width = unit(.5, "in")
      ) +
      labs(fill = "Ticks Collected")
  })

  output$Age <- renderPlotly({
    rds_data %>% 
      year_data %>%
      select(year, age, cases) %>%
      filter(year == "input$year") %>%
      ggplot(mapping = aes(x = age, y = cases)) +
      geom_col()
  })
}

# Run the application
shinyApp(ui = ui, server = server)