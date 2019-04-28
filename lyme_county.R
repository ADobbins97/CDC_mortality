library(tidyverse)
library(ggplot2)
library(urbnmapr)
library(urbnthemes)
library(readr)
library(janitor)
library(gt)

lyme <- read_csv("LD-Case-Counts-by-County-00-17.csv") %>% 
  clean_names()

ca_county <- read_csv("infectious-disease-cases-by-county-year-and-sex-2-27-19.csv") %>% 
  clean_names() %>% 
  filter(disease == "Lyme Disease")
co_nymph <- read_csv("Deer_Tick_Surveillance__Nymphs__May_to_Sept__excluding_Powassan_virus__Beginning_2008.csv") %>% 
  clean_names() %>% 
  filter(county_centroid != "40 6546")
co_adult <- read_csv("Deer_Tick_Surveillance__Adults__Oct_to_Dec__excluding_Powassan_virus__Beginning_2008.csv") %>% 
  clean_names()

lyme %>% 
  select(stname, cases2000, cases2001, cases2002, cases2003, cases2004, cases2005, cases2006, cases2007, cases2008, cases2009, cases2010, cases2011, cases2012, cases2013, cases2014, cases2015, cases2016, cases2017) %>% 
  group_by(stname) %>% 
  summarize(total = n()) %>% 
  arrange(desc(total)) %>%
  gt() %>% 
  cols_label(stname = "State", 
             total = "Recorded Cases")

  lyme %>% 
  group_by(stname) %>% 
  summarize(sum_2000 = sum(cases2000), sum_2001 = sum(cases2001), sum_2002 = sum(cases2002), sum_2003 = sum(cases2003), sum_2004 = sum(cases2004), sum_2005 = sum(cases2005), sum_2006 = sum(cases2006), sum_2007 = sum(cases2007), sum_2008 = sum(cases2008), sum_2009 = sum(cases2009), sum_2010 = sum(cases2010), sum_2011 = sum(cases2011), sum_2012 = sum(cases2012), sum_2013 = sum(cases2013), sum_2014 = sum(cases2014), sum_2015 = sum(cases2015), sum_2016 = sum(cases2016), sum_2017 = sum(cases2017), sum_all = sum(c(cases2000, cases2001, cases2002, cases2003, cases2004, cases2005, cases2006, cases2007, cases2008, cases2009, cases2010, cases2011, cases2012, cases2013, cases2014, cases2015))) %>%
  arrange(desc(sum_all)) %>% 
  select(stname, sum_2000, sum_2001, sum_2002, sum_2003, sum_2004, sum_2005, sum_2006, sum_2007, sum_2008, sum_2009, sum_2010, sum_2011, sum_2012, sum_2013, sum_2014, sum_2015, sum_2016, sum_2017) %>% 
  gt() %>% 
  cols_label(stname = "State", 
             sum_2000 = "2000", 
             sum_2001 = "2001", 
             sum_2002 = "2002",
             sum_2003 = "2003",
             sum_2004 = "2004",
             sum_2005 = "2005", 
             sum_2006 = "2006",
             sum_2007 = "2007",
             sum_2008 = "2008",
             sum_2009 = "2009",
             sum_2010 = "2010",
             sum_2011 = "2011",
             sum_2012 = "2012",
             sum_2013 = "2013",
             sum_2014 = "2014",
             sum_2015 = "2015",
             sum_2016 = "2016",
             sum_2017 = "2017")
#turn this into a line graph that traces itself and maybe hover in which you select state to see the evolution of lyme cases  
  lyme %>% 
    group_by(ctycode) %>% 
    summarize(total = n()) %>% 
    arrange(desc(total)) %>% 
    slice(1:25)
  