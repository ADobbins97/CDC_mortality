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

lyme <- read_csv("LD-Case-Counts-by-County-00-17.csv") %>%
  clean_names()

ca_county <- read_csv("infectious-disease-cases-by-county-year-and-sex-2-27-19.csv") %>%
  clean_names() %>%
  filter(disease == "Lyme Disease")

co_nymph <- read_csv("Deer_Tick_Surveillance__Nymphs__May_to_Sept__excluding_Powassan_virus__Beginning_2008.csv") %>%
  clean_names() %>%
  filter(county_centroid != "40 6546") %>%
  select(year,
    "county_name" = county, total_ticks_collected, tick_population_density, b_burgdorferi_percent,
    a_phagocytophilum_percent, b_microti_percent, b_miyamotoi_percent, county_centroid
  )

co_adult <- read_csv("Deer_Tick_Surveillance__Adults__Oct_to_Dec__excluding_Powassan_virus__Beginning_2008.csv") %>%
  clean_names()

# Define UI for application that draws a histogram
ui <- navbarPage(
  "Lyme Disease: the United States",
  theme = shinytheme("cosmo"),

  tabPanel(
    "Main",
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
          It can often be difficult to identify someone has lyme in the absence of visible physical markers. Thus, the data here is only the **identified likely** cases which are reported to the CDC.")
      )
    )
  ),

  tabPanel(
    "State: New York",
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
    "National",
    fluidPage(
      fluidRow(
        h1("Reported Cases of Lyme Disease per State, from 2000 to 2017"),
        br(),
        sidebarLayout(
          sidebarPanel(
            mainPanel(plotOutput("USMap", width = 500, height = 500))
          ),
          position = "left",
          pickerInput("year", "Year", choices = c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"), selected = c("2000"), options = list(`actions-box` = TRUE), multiple = FALSE)
          # Show a plot of the generated distribution
        )
      )
    )
  )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  output$USMap <- renderPlot({
    state_cases <-
      lyme %>%
      group_by(stname) %>%
      summarize("2000" = sum(cases2000), "2001" = sum(cases2001), "2002" = sum(cases2002), "2003" = sum(cases2003), "2004" = sum(cases2004), "2005" = sum(cases2005), "2006" = sum(cases2006), "2007" = sum(cases2007), "2008" = sum(cases2008), "2009" = sum(cases2009), "2010" = sum(cases2010), "2011" = sum(cases2011), "2012" = sum(cases2012), "2013" = sum(cases2013), "2014" = sum(cases2014), "2015" = sum(cases2015), "2016" = sum(cases2016), "2017" = sum(cases2017), sum_all = sum(c(cases2000, cases2001, cases2002, cases2003, cases2004, cases2005, cases2006, cases2007, cases2008, cases2009, cases2010, cases2011, cases2012, cases2013, cases2014, cases2015))) %>%
      arrange(desc(sum_all)) %>%
      select(stname, "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")

    state_cases <- gather(state_cases, "2000", "2001", "2002", "2003", "2004", "2005", "sum_2006", "sum_2007", "sum_2008", "sum_2009", "sum_2010", "sum_2011", "sum_2012", "sum_2013", "sum_2014", "sum_2015", "sum_2016", "sum_2017",
      key = "year", value = "cases"
    )

    state_cases %>%
      ggplot() +
      geom_polygon(
        data = urbnmapr::states, mapping = aes(x = long, y = lat, group = group),
        fill = "#a2d4ec", color = "white"
      ) +
      geom_text(
        data = get_urbn_labels(map = "states"), aes(x = long, lat, label = state_abbv),
        size = 2
      ) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45)
  })

  output$NYPlot <- renderPlot({
    # need to layer this so outline of new york doesn't disappear
    x <- urbn_data
    y <- nymph_tick_number %>% filter(year == input$year) %>% select(county_name, total_ticks)
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
}

# Run the application
shinyApp(ui = ui, server = server)