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
library(shinydashboard)
library(tidyverse)
library(maps)
library(rgeos)

#Open datasets to be used for rest of app
urbn_data <-
  urbnmapr::counties %>%
  filter(state_abbv == "NY") %>%
  mutate(county_name = removeWords(county_name, " County")) %>%
  group_by(county_name)

read_rds("app2/rds_files /cdc_worked_data.rds")

#Setting up visual aspect of app

ui <- fluidPage(
  theme = shinytheme("cosmo"),

  navbarPage(
    "Lyme Disease in the United Staes: the historical perspective",
    
#
##
###
##
#


    tabPanel(
      "Home",
      sidebarPanel(
        #4,
        wellPanel(
          h3("About the Analyst"),
          br(),
          h5("Alexandra Dobbins is a junior at Harvard College.")
        )
      ),
      mainPanel(
        titlePanel(
          h3("Lyme Disease: Statistics and History in the United States"),
          h5(
            p("Every year, health care providers around the country report likely cases to the CDC. They, in turn assimilate this information into reports and datasets which are available to the public. However, because states and organizations report independently, the types of data is not always consistent and therefore in this study, only New York will be analyzed on a state-level for the sake of continuity."),
            br(),
            p("Every year, up to 300,000 cases of Lyme disease are reported. The disease is caused by a bacterium called Borrelia burgdoferi. It is passed to humans by ticks, although not all types of ticks carry it. After being bitted a `bulls-eye` circle appears around the bite site and this is the first warning sign you might have contracted Lyme. The problem is that sometimes people do not find the bite, and think they have a cold -- fever, headache, fatigue."),
            br(),
            p("However, symptoms quickly progress beyond the flu-like symptoms and the disease can be fully debilitating. Untreated, it will spread to one's joints, heart, and nervous system and cause a multitude of side effects: 
                                            Extreme fatigue, memory loss, mood swings, depression, anxiety, headaches, muscle fatigue, bone pain, abdominal disruption, bloating, digestive issues, loss of appetite, facial swelling, thyroid disease, and many more.
                                            It can often be difficult to identify someone has lyme in the absence of visible physical markers. Thus, the data here is only the identified likely cases which are reported to the CDC.")
          )
        )
      )
    ),

#
##
###
##
#

    tabPanel(
      "National Trends",
      dashboardBody(
        fluidRow(
          box(
            mainPanel(
              titlePanel(
                h3("Total State-Reported Cases of Lyme Disease Since 2010"),
                plotlyOutput("US_Reported_Static")
              )
            ),
            selectInput("year", label = "Year", choices = list(
              "2008", "2009", "2010", "2011", "2012", "2013", "2014",
              "2015", "2016", "2017"
            ), selected = "2000", multiple = FALSE)
          ),
          box(
            wellPanel(
              h3("Where and When"),
              h4("Reported Cases of Lyme Disease by State and Year"),
              br(),
              h5("Use the selection bar to choose the year and hover fro specific statistics")
            ),
            mainPanel(
              h3("Total State-Reported Cases of Lyme Disease since 2010"),
              h4("Organized by State and by Year"),
              plotlyOutput("US_Reported_Yearly")
            )
          )
        )
      )
    ),
# 
# #
# ##
# ###
# ##
# #
#     

    tabPanel(
      "Population Distributions",
      dashboardBody(
        fluidRow(
          box(
            wellPanel(
              h3("Choose a Year"),
              h5("Use the drop-down menu to select a year, then explore the number of cases reported by age, race, and gender."),
              # change select input to multi-selecter thing. slider?
              selectInput("year", label = "Year", choices = list(
                "2008", "2009", "2010", "2011", "2012", "2013", "2014",
                "2015", "2016", "2017"
              ), selected = "2000", multiple = FALSE)
            )
          ),
          box(
            mainPanel(
              tabsetPanel(
                tabPanel(
                  "Age",
                  h4("Reported Cases of Lyme Disease by Age Group"),
                  h5("Year: "), # make this reflect the year chosen?
                  br(),
                  plotOutput("Barplot_Age"),
                  br(),
                  h5("insert brief observation of data")
                ),
                tabPanel(
                  "Race",
                  h4("Reported Cases of Lyme Disease by Race"),
                  h5("Year: "), # make this reflect the year chosen?
                  br(),
                  plotOutput("Barplot_Race"),
                  br(),
                  h5("insert brief observation about data")
                ),
                tabPanel(
                  "Gender",
                  h4("Reported Cases of Lyme Disease by Gender"),
                  h5("Year: "), # make this reflect the year chosen?
                  br(),
                  plotOutput("Barplot_Gender"),
                  br(),
                  h5("insert brief observation about data")
                )
              )
            )
          )
        )
      )
    ),

#     
# #
# ##
# ###
# ##
# #

    tabPanel(
      "Biological Information",
      dashboardBody(
        fluidRow(
          box(
            wellPanel(
              h3("Choose a Year"),
              h5("Use the drop-down menu to select a year, then hover over the New York counties to see the distribution of bacteria found in ticks collected and studied by a research team each year."),
              # change select input to multi-selecter thing. slider?
              selectInput("year", label = "Year", choices = list(
                "2008", "2009", "2010", "2011", "2012", "2013", "2014",
                "2015", "2016", "2017"
              ), selected = "2000")
            )
          ),

          box(
            mainPanel(
              h4("Occurrence of Four Types of Bacteria Found in New York"),
              h5("as identified off samples collected from varying counties since 2000"),
              br(),
              plotlyOutput("NY_Bacteria"),
                br(),
                h5("Brief description of the data observed")
              )
            )
          )
        )
     )
   )
)


server <- function(input, output) {



}

# Run the application
shinyApp(ui = ui, server = server)