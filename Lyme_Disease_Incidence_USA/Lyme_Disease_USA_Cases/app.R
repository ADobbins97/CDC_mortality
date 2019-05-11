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
library(viridis)

# Urbndata is a package used to create extra-aesthetic maps. I chose to use it because it had really clear instructions and looked great right off the bat, but ultiamtely it prevented me from using hover features because it doesn't play well with plotly (at least not that I could figure out in this short time. )
#This dataset being read in is for New York state and includes county lines. 
urbn_data <-
  urbnmapr::counties %>%
  filter(state_abbv == "NY") %>%
  mutate(county_name = removeWords(county_name, " County")) %>%
  group_by(county_name)


#The rest of the files I downloaded and used for this project are cleaned and manipualted in this file, which is read in now and referred to for the rest of the project. 
read_rds("rds_files /cdc_worked_data.rds")

# Setting up visual aspect of app

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
      
      #In order to open on an interesting page, I put the Naitonal Trends tab first to give a big-picture perspective before narrowing in and portraying specifics 
      "National Trends",
      
      dashboardBody(
      
          fluidRow(
        
              box(
            
                mainPanel(
              
                  titlePanel(
               
                     h3("Total State-Reported Cases of Lyme Disease Since 2010")
                    ),
                
                  plotlyOutput("US_Reported_Static")
                  
              )
      
          ),
          
          box(
            
            mainPanel(
              
              titlePanel(
                
                h3("State-Reported Cases of Lyme Disease by Year")
              ),
              
              plotlyOutput("US_Reported_Yearly"),
          
          selectInput("year", label = "Year", choices = list(
            "2008", "2009", "2010", "2011", "2012", "2013", "2014",
            "2015", "2016", "2017"
            
          ), selected = "2000", multiple = FALSE)
          
        )
      )
    )
    )
    ),

    #
    # #
    # ##
    # #
    #


    tabPanel(
      
    #The next tab will allow the viewer to delve deeper into the specific trends relating to age, gender, and race. For this page, I create one year-selector, and then three subtabs so that someone can pick a year and quickly click through the qualities of that year from the three different perspectives. 
      #In addition, they can click through the different years looking at the same tab as well. Makes the data visualization versatile and easy to compare and explore. 
      "Population Distributions",
      
      dashboardBody(
        
        fluidRow(
          
          box(
            
                      wellPanel(
            
                          h3("Choose a Year"),
 
                          h5("Use the drop-down menu to select a year, then explore the number of cases reported by age, race, and gender."),
             
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
                
                              br(),
                 
                             plotOutput("Barplot_Age"),
                 
                             br(),
                 
                             h5("Individuals between 40 and 64 are seen to be most affected by Lyme disease. In addition, the overall number of cases reported and recorded has risen steadily. This could be due to either an increased awareness and tendency to diagnose, or becaue of more actual cases. ")
                ),
              
                  tabPanel(
               
                       "Race",
               
                          h4("Reported Cases of Lyme Disease by Race"),
                 
                        br(),
                 
                        plotOutput("Barplot_Race"),
                 
                        br(),
                  h5("This graph is on a log scale. White individuals are most affected by lyme disease, white Alskans and Pacific Islanders are extremely low. This is not unsurprising because Lyme Disease originated and is most concentrated in New England.")
                
                  ),
                
                tabPanel(
                  
                  "Gender",
                  
                  h4("Reported Cases of Lyme Disease by Gender"),

                  br(),
                  
                  plotOutput("Barplot_Gender"),
                 
                   br(),
                  
                  h5("Gender-based data shows a less consistent trend, but notice how in recent years there have been many more cases in women than in men.")
                )
              )
            )
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
      
      "Biological Information",
      
      #I found some really great data about the types of bacteria found on ticks in New York. However, I struggled to add it to the map in a concise and aesthetic way, so I created the data table below to let viewers easily see and explore the percentages of each bacteria. 
      dashboardBody(
        
        fluidRow(
          
          box(
            
            wellPanel(
              
              h3("Choose a Year"),
              
              h5("Use the drop-down menu to select a year, then hover over the New York counties to see the distribution of bacteria found in ticks collected and studied by a research team each year."),

              selectInput("year", label = "Year", choices = list(
                "2008", "2009", "2010", "2011", "2012", "2013", "2014",
                "2015", "2016", "2017"
              ), selected = "2000")
            
              )
          
            ),

          box(
            
            mainPanel(
              
              tabsetPanel(
                
                tabPanel(
                  
                  h4("Occurrence of Four Types of Bacteria Found in Adult Ticks"),
                  
                  h5("as identified off samples collected from varying parts of New York since 2000"),
                  
                  br(),
                  
                  plotlyOutput("NY_Bacteria_Nymph"),
                  
                  plotOutput("Nymph_Table"),

                  tabPanel(
                  
                      h4("Occurrence of Four Types of Bacteria Found in Nymph Ticks"),
                  
                        h5("as identified off samples collected from varying parts of New York since 2000"),
                  
                        br(),
                   
                       plotlyOutput("NY_Bacteria_Adult"),
                      
                      plotOutput("Adult_Table")
                  )
                )
              )
            )
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
     
       "About",
      
      sidebarPanel(
       
       
         wellPanel(
         
            h3("About the Analyst"),
          
            br(),
          
            h5("Alexandra Dobbins is a junior at Harvard College. She studies psychology and archaeology, but her recent exposure to R has sparked a new interest for her."),
         
             br(),
          
            h5("Check out her GitHub at https://github.com/ADobbins97 !"),
          
            br(),
          
            h5("Or, contact her at alexandra_dobbins@college.harvard.edu")
        )
      ),
      
      mainPanel(
        
        titlePanel(
          
          mainPanel(
            
            h3("Lyme Disease: Statistics and History in the United States"),
            
            h5(
             
               p("Every year, health care providers around the country report likely cases to the CDC. They, in turn assimilate this information into reports and datasets which are available to the public. However, because states and organizations report independently, the types of data is not always consistent and therefore in this study, only New York will be analyzed on a state-level for the sake of continuity."),
              
               br(),
              
               p("Every year, up to 300,000 cases of Lyme disease are reported. The disease is caused by a bacterium called Borrelia burgdoferi. It is passed to humans by ticks, although not all types of ticks carry it. After being bitted a `bulls-eye` circle appears around the bite site and this is the first warning sign you might have contracted Lyme. The problem is that sometimes people do not find the bite, and think they have a cold -- fever, headache, fatigue."),
              
               br(),
              
                p("However, symptoms quickly progress beyond the flu-like symptoms and the disease can be fully debilitating. Untreated, it will spread to one's joints, heart, and nervous system and cause a multitude of side effects: 
          Extreme fatigue, memory loss, mood swings, depression, anxiety, headaches, muscle fatigue, bone pain, abdominal disruption, bloating, digestive issues, loss of appetite, facial swelling, thyroid disease, and many more.
          It can often be difficult to identify someone has lyme in the absence of visible physical markers. Thus, the data here is only the identified likely cases which are reported to the CDC."),
                br(),
                p("Source: https://www.cdc.gov/lyme/index.html")
              )
          )
        )
      )
    )
  )
)


server <- function(input, output) {
  
  output$US_Reported_Static <- renderPlotly({
    
    ggplot(data = shapes2) +
      
      geom_sf(mapping = aes(fill = avg_cases)) +
      
      labs(
        
        fill = "Unit",
        
        title = "USA Tick Data"
        
      )
  })

  output$US_Reported_Yearly <- renderPlotly({
    
    shapes <- shapes %>% filter(year == "input$year")

    us_cases_map <-
      
      ggplot(data = shapes) +
      
      geom_sf(mapping = aes(fill = cases)) +
      labs(
        
        fill = "Reported Cases",
        
        caption = "Source: CDC"
        
      ) +
      xlab(" ") +
      
      ylab(" ")
    
  #I was SO CLOSE to getting this to work. It was an effort to switch over to plotly, so I could have a simpler hover because I made no progress for days on HoverOpts. And I made the ocuntry outline, and the hover, but I had trouble with the volume gradient because the coordinates were non-transferable -- in Urbnmapr I joined by county name, but that wouldn't work here. With more time and less of *other* workload I would've made this work but i think I'd have to go back to square one essentially.   
    # us_cases_map %>%
    #   ggplotly(tooltip = "text") %>%
    #   style(hoverlabel = list(bgcolor = "white"), hoveron = "fill")
  })


  output$NY_Bacteria_Nymph <- renderPlot({
    
    #There are two maps of New York. One is for adult and one for nymphs, so I create these as separate but identically formatted maps with teh package Urbnmapr. This was very hard because the fill was tied to the outline of the counties, so when there was NA input for a county, it would disappear and this was very bad. Thus, I joined an empty set of the urban mao data with the normal layer, with NAs made into 0s so that the map was consistent throughout years.   
    
    x1 <- urbn_data
    
    y1 <- ny_tick_nymphs %>% filter(year == "input$year")

    left_join(x1, y1, by = "county_name") %>%
      
      ggplot(mapping = aes(long, lat, group = group, fill = total_ticks)) +
      
      geom_polygon(color = "white", size = .25) +
      
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      
      theme(
        
        legend.title = element_text(),
        
        legend.key.width = unit(.5, "in")
        
      ) +
      
      labs(fill = "Ticks Collected")
  })

#Exactly the same as above map, from different dataset  
  
  output$NY_Bacteria_Adult <- renderPlot({
    
    
    x2 <- urbn_data
    
    y2 <- ny_adult %>% filter(year == "input$year")

    left_join(x2, y2, by = "county_name") %>%
      
      ggplot(mapping = aes(long, lat, group = group, fill = total_ticks)) +
      
      geom_polygon(color = "black", size = .25) +
      
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      
      theme(
        
        legend.title = element_text(),
        
        legend.key.width = unit(.5, "in")
        
      ) +
      
      labs(
        
        title = "",
        
        fill = "Ticks Collected"
        
      )
  })


  output$Barplot_Age <- renderPlot({
    
    #The next three plots are exactly the same, they take data from a file which has all the types of data as columns, thus I had to gather each set before plotting it as a geom_col. I had to use geom_col because the number of cases was recorded as a cell input, and bar/histogram count() columns rather than recording cell data. 
    #This, we have the bar plots found below, with a selector for each that controlls year being presented. 
    
    year_data_age %>%
      
      select(year, age, cases) %>%
      
      filter(year == "input$year")
    
    ggplot() +
      
      #Replaced the gray scale with a nice couple shades of blue to clearly distinguish the columns. 
      
      geom_col(mapping = aes(x = age, y = cases, fill = ..x..)) +
      
      coord_flip() +
      
      xlab("Age") +
      
      ylab("Reported Cases") +
      
      labs(
        
        title = "National Reported Cases of Lyme Disease by Age Group",
        
        caption = "Source: CDC"
        
      )
  })

  output$Barplot_Race <- renderPlot({
    
    year_data_race %>%
      
      select(year, race, cases) %>%
      
      filter(year == "input$year") %>%
      
      ggplot() +
      
      geom_col(mapping = aes(x = race, y = cases, fill = ..x..)) +
      
      coord_flip() +
      
      xlab("Race") +
      
      ylab("Reported Cases") +
      
      labs(
        
        title = "National Reported Cases of Lyme Disease by Race",
        
        caption = "Source: CDC"
        
      )
  })

  output$Barplot_Gender <- renderPlot({
    
    year_data_gender %>%
      
      filter(year == "input$year") %>%
      
      select(year, gender, cases) %>%
      
      ggplot() +
      
      geom_col(mapping = aes(x = gender, y = cases, fill = ..x..)) +
      
      xlab("Gender") +
      
      ylab("Reported Cases") +
      
      labs(
        
        title = "National Reported Cases of Lyme Disease by Gender",
        
        caption = "Source: CDC"
        
      )
  })
  
output$Nymph_Table <- renderDataTable({
  
  nymph_table %>%  filter(year == "input$year") %>% 
    gt()
  
})

output$Adult_Table <- renderDataTable({
  
  adult_table %>%  filter(year == "input$year") %>% 
    gt()
  
})
}

# Run the application
shinyApp(ui = ui, server = server)