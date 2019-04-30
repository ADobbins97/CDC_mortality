library(tidyverse)
library(ggplot2)
library(urbnmapr)
library(urbnthemes)
library(readr)
library(janitor)
library(gt)
library(leaflet)
library(shiny)

lyme <- read_csv("LD-Case-Counts-by-County-00-17.csv") %>% 
  clean_names()


state_cases <-
lyme %>% 
  select(stname, cases2000, cases2001, cases2002, cases2003, cases2004, cases2005, cases2006, cases2007, cases2008, cases2009, cases2010, cases2011, cases2012, cases2013, cases2014, cases2015, cases2016, cases2017) %>% 
  group_by(stname) %>% 
  summarize(total = n()) %>% 
  arrange(desc(total)) %>%
  gt() %>% 
  cols_label(stname = "State", 
             total = "Recorded Cases")

state_cases <-
  lyme %>% 
  group_by(stname) %>% 
  summarize("2000" = sum(cases2000), "2001" = sum(cases2001), "2002" = sum(cases2002), "2003" = sum(cases2003), "2004" = sum(cases2004), "2005" = sum(cases2005), "2006" = sum(cases2006), "2007" = sum(cases2007), "2008" = sum(cases2008), "2009" = sum(cases2009), "2010" = sum(cases2010), "2011" = sum(cases2011), "2012" = sum(cases2012), "2013" = sum(cases2013), "2014" = sum(cases2014), "2015" = sum(cases2015), "2016" = sum(cases2016), "2017" = sum(cases2017), sum_all = sum(c(cases2000, cases2001, cases2002, cases2003, cases2004, cases2005, cases2006, cases2007, cases2008, cases2009, cases2010, cases2011, cases2012, cases2013, cases2014, cases2015))) %>%
  arrange(desc(sum_all)) %>% 
  select(stname, "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
  
  
  
state_cases <- gather(state_cases, "2000", "2001", "2002", "2003", "2004", "2005", "sum_2006", "sum_2007", "sum_2008", "sum_2009", "sum_2010", "sum_2011", "sum_2012", "sum_2013", "sum_2014", "sum_2015", "sum_2016", "sum_2017",
       key = "year", value = "cases")

#turn this into a line graph that traces itself and maybe hover in which you select state to see the evolution of lyme cases  
  lyme %>% 
    group_by(ctycode) %>% 
    summarize(total = n()) %>% 
    arrange(desc(total)) %>% 
    slice(1:25)

  
state_cases %>%    
ggplot() + 
  geom_polygon(data = urbnmapr::states, mapping = aes(x = long, y = lat, group = group),
               fill = "#a2d4ec", color = "white") +
  geom_text(data = get_urbn_labels(map = "states"), aes(x = long, lat, label = state_abbv), 
            size = 2) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)
  
  
  