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

ca_county <- read_csv("infectious-disease-cases-by-county-year-and-sex-2-27-19.csv") %>% 
  clean_names() %>% 
  filter(disease == "Lyme Disease")

co_nymph <- read_csv("Deer_Tick_Surveillance__Nymphs__May_to_Sept__excluding_Powassan_virus__Beginning_2008.csv") %>% 
  clean_names() %>% 
  filter(county_centroid != "40 6546") %>% 
  select(year, "county_name" = county, total_ticks_collected, tick_population_density, b_burgdorferi_percent, 
         a_phagocytophilum_percent, b_microti_percent, b_miyamotoi_percent, county_centroid)

co_adult <- read_csv("Deer_Tick_Surveillance__Adults__Oct_to_Dec__excluding_Powassan_virus__Beginning_2008.csv") %>% 
  clean_names()

#######


nymph_tick_number <-  
  co_nymph %>%
  separate(county_centroid, c("latitude", "longitude"), sep = ",") %>% 
  mutate(latitude = substring(latitude, 2,11)) %>% 
  mutate(longitude = substring(longitude, 1,10)) %>% 
  group_by(county_name, year, latitude, longitude) %>% 
  mutate(total_ticks = sum(total_ticks_collected)) %>% 
  mutate(total_ticks = log(total_ticks, b = 10))
  

# nymph_location <-
#   st_as_sf(nymph_tick_number,
#            coords = c("longitude", "latitude"),
#            crs = 4326)

# shapes <- 
#   urban_areas(class = "sf") 
# 
# shapes <-
#   shapes %>%
#   clean_names() %>% 
#   separate(name10, c("city", "state"), sep = ",") %>% 
#   filter(state == " NY")

urbn_data <-
  urbnmapr::counties %>%
    filter(state_abbv == "NY") %>%
    mutate(county_name = removeWords(county_name, " County")) %>%
    group_by(county_name)


# dataset <- 
# full_join(nymph_tick_number, urbn_data, by="county_name")

# ggplot(data = shapes) +
#   geom_sf() + 
#   geom_sf(data = nymph_location, mapping = aes(color = total_ticks))+
#   labs(fill = "Ticks", 
#        title = "Ticks in Counties in New York")

# ggplot() + 
#   geom_polygon(data = urbn_data,
#                mapping = aes(long, lat, color = "white")) +
#   geom_sf(data = nymph_location, aes(color = "black", size = 0.1)) + 
#   scale_fill_gradient(labels = scales::percent,
#                       guide = guide_colorbar(title.position = "top"))
#   


nymph_tick_number %>% 
  left_join(urbn_data, by = "county_name") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = total_ticks)) +
  geom_polygon(color = "white", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(title = "",
       fill = "Ticks Collected")
# 

View(nymph_tick_number %>% 
       left_join(urbn_data, by = "county_name"))
# dataset <- 
#   right_join(nymph_tick_number, urbn_data, by="county_name") %>% 
#     ggplot(mapping = aes(long, lat, group = group, fill = total_ticks))
#      geom_polygon(color = "white", size = 0.4) +
#      geom_sf(data = nymph_location, aes(fill = total_ticks))
#      scale_fill_gradientn(labels = scales::number,
#                        guide = guide_colorbar(title.position = "top")) 

  


# ggplot(mapping = aes(long, lat, group = group, fill = horate)) +
#   geom_polygon(color = "#ffffff", size = .25)
# 
#     geom_polygon(data = nymph_tick_number,
#                  mapping = aes(longitude, latitude, color = "total_ticks"))
# 
# 
#   
#   group_by(county, year) %>% 
#     #mutate(latitude = mean(latitude), longitude = mean(longitude))
#     summarize(total_ticks = sum(total_ticks_collected))  
#   spread(key = year, value = total_ticks)



