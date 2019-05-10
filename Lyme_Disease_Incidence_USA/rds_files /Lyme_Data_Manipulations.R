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
library(readxl)
library(maps)
library(rgeos)


#New York Maps of Adult/Nymph Bacteria by County
ny_nymphs <- read_csv("rds_files /Deer_Tick_Surveillance__Nymphs__May_to_Sept__excluding_Powassan_virus__Beginning_2008.csv") %>% 
  clean_names() %>% 
  filter(county_centroid != "40 6546") %>% 
  select(year, "county_name" = county, total_ticks_collected, tick_population_density, b_burgdorferi_percent, 
         a_phagocytophilum_percent, b_microti_percent, b_miyamotoi_percent, county_centroid)

ny_tick_nymphs <-
  ny_nymphs %>% 
  separate(county_centroid, c("latitude", "longitude"), sep = ",") %>% 
  mutate(latitude = substring(latitude, 2,10)) %>% 
  mutate(longitude = substring(longitude, 1,9)) %>% 
  group_by(county_name, year, latitude, longitude) %>% 
  mutate(total_ticks = sum(total_ticks_collected)) %>% 
  mutate(total_ticks = log(total_ticks, b = 10))


nymph_tick_ny_map <-
  nymph_tick_number %>% 
  left_join(urbn_data, by = "county_name") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = total_ticks)) +
  geom_polygon(color = "white", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(title = "",
       fill = "Ticks Collected")

nymph_table <-
  ny_nymphs %>% 
  select(year, county_name, b_burgdorferi_percent, a_phagocytophilum_percent, b_microti_percent, b_miyamotoi_percent)
  

#
##
###
##
#

ny_adult <- read_csv("rds_files /Deer_Tick_Surveillance__Adults__Oct_to_Dec__excluding_Powassan_virus__Beginning_2008.csv") %>% 
  clean_names() %>% 
  select(year, "county_name" = county, total_ticks_collected, tick_population_density, b_burgdorferi_percent, 
         a_phagocytophilum_percent, b_microti_percent, b_miyamotoi_percent, county_centroid)

adult_tick_number <-  
  ny_adult %>%
  separate(county_centroid, c("latitude", "longitude"), sep = ",") %>% 
  mutate(latitude = substring(latitude, 2,10)) %>% 
  mutate(longitude = substring(longitude, 1,10)) %>% 
  group_by(county_name, year, latitude, longitude) %>% 
  mutate(total_ticks = sum(total_ticks_collected)) %>% 
  mutate(total_ticks = log(total_ticks, b = 10))


adult_tick_ny_map <-
adult_tick_number %>% 
  left_join(urbn_data, by = "county_name") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = total_ticks)) +
  geom_polygon(color = "black", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(title = "",
       fill = "Ticks Collected")


#
##
###
##
#


lyme <- read_csv("LD-Case-Counts-by-County-00-17.csv") %>% 
  clean_names()

state_cases <-
  lyme %>% 
  group_by(stname) %>% 
  summarize("2000" = sum(cases2000), "2001" = sum(cases2001), "2002" = sum(cases2002), "2003" = sum(cases2003), "2004" = sum(cases2004), "2005" = sum(cases2005), "2006" = sum(cases2006), "2007" = sum(cases2007), "2008" = sum(cases2008), "2009" = sum(cases2009), "2010" = sum(cases2010), "2011" = sum(cases2011), "2012" = sum(cases2012), "2013" = sum(cases2013), "2014" = sum(cases2014), "2015" = sum(cases2015), "2016" = sum(cases2016), "2017" = sum(cases2017), sum_all = sum(c(cases2000, cases2001, cases2002, cases2003, cases2004, cases2005, cases2006, cases2007, cases2008, cases2009, cases2010, cases2011, cases2012, cases2013, cases2014, cases2015))) %>%
  arrange(desc(sum_all)) %>% 
  select("ID" = stname, "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017") %>% 
  mutate(ID = tolower(ID))


state_cases <- gather(state_cases, "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017",
                      key = "year", value = "cases") %>% 
  mutate(cases = log(cases, b = 10))

hapes <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

shapes <- left_join(state_cases, hapes, by = "ID")

ggplot(data = shapes) +
  geom_sf(mapping = aes(fill = cases))+
  labs(fill = "Unit", 
       title = "USA Tick Data")


#
##
###
##
#

state_cases_yearmap <-
  lyme %>% 
  group_by(stname) %>% 
  summarize("2000" = sum(cases2000), "2001" = sum(cases2001), "2002" = sum(cases2002), "2003" = sum(cases2003), "2004" = sum(cases2004), "2005" = sum(cases2005), "2006" = sum(cases2006), "2007" = sum(cases2007), "2008" = sum(cases2008), "2009" = sum(cases2009), "2010" = sum(cases2010), "2011" = sum(cases2011), "2012" = sum(cases2012), "2013" = sum(cases2013), "2014" = sum(cases2014), "2015" = sum(cases2015), "2016" = sum(cases2016), "2017" = sum(cases2017), sum_all = sum(c(cases2000, cases2001, cases2002, cases2003, cases2004, cases2005, cases2006, cases2007, cases2008, cases2009, cases2010, cases2011, cases2012, cases2013, cases2014, cases2015))) %>%
  arrange(desc(sum_all)) %>% 
  select("state_name" = stname, "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")


state_cases_yearmap <- gather(state_cases, "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017",
                      key = "year", value = "cases") %>% 
  mutate(cases = log(cases, b = 10))



#Dataset of Cases per Age by Year 
yearly_data <- read_xlsx("age_race_lyme_disease_data.xlsx") %>% 
  clean_names() 

year_data <- gather(yearly_data, "under_1_yr", "x1_4_yr", "x5_14_yr", "x15_24_yr", "x25_39_yr", "x40_64_yr", "greater_65_yr", key = "age", value = "cases")

year_data %>%
  select(year, age, cases) %>%
  filter(year == "2016") %>%
  ggplot(mapping = aes(x = age, y = cases)) +
  geom_col()


#Dataset for 


write_rds("Lyme_Data_Manipulation", "rds_files /cdc_worked_data.rds")
