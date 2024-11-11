# ARCHIVED CODE
library(tidyverse)
library(sf)
library(geojsonsf)
library(matrixStats)
source("redliningp1.R")

# Number of census tracts in each HOLC category
mi_doe_redlining %>%
  group_by(category) %>%
  count() %>%
  ungroup()

census_holc <- mi_doe_redlining %>% 
  #taking the gi and category columns into temp
  select(gi, category) %>% 
  #removing columns that have na values for category and gi
  filter(!is.na(category) & !is.na(gi)) %>% 
  #taking 1/5 of the columns, removing duplicates
  distinct() %>% 
  #grouping by category
  group_by(category) %>% 
  #counting the number of gi that are in that category
  tally() %>% 
  #ungrouping the data
  ungroup() 

ggplot(census_holc, aes(x=category, y=n)) + 
  #making a bar chart
  geom_bar(stat = "identity") +
  # setting x label to "HOLC Category"
  xlab("HOLC Category") +
  #setting y label to "Number of Census Tracts"
  ylab("Number of Census Tracts")

