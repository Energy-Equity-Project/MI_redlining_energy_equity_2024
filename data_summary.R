# Libraries---------------------------------------------------------------------
library(tidyverse)
library(sf)
library(geojsonsf)
library(matrixStats)
# source("redliningp1.R")
outputs_dir <- "outputs"
mi_doe_redlining <- read.csv(file.path(outputs_dir, "mi_doe_redlining.csv"))

# Summary Stats-----------------------------------------------------------------
# Summary statistics on mi_doe_redlining dataframe
# Note: gi = census tract number, h_count = number of households, burden = utility/energy burden, income = household income

#thinking abt number of household instead of %, household count vs census tract

mi_doe_redlining <- mi_doe_redlining %>%
  mutate(category = factor(
    category,
    levels = c("Best", "Still Desirable", "Definitely Declining", "Hazardous", "Commercial", "Industrial", "Industrial and Commercial")
  ))

mi_doe_redlining <- mi_doe_redlining %>%
  filter(!(category %in% c("Commercial", "Industrial", "Industrial and Commercial")))


# Number of households in each HOLC category
households_holc <- mi_doe_redlining %>%
  # taking gi, category, and h_count columns
  select(gi, category, h_count) %>%
  # removing rows with na values for gi,cateogry, or h_count
  filter(!is.na(category) & !is.na(gi) & !is.na(h_count)) %>% 
  # grouping by category
  group_by(category) %>%
  # sum to find # of houses for each category
  summarise(sum_h = sum(h_count, na.rm = TRUE)) %>%
  # ungrouping from category
  ungroup() %>%
  # adding column of % of houses in each category of total
  mutate(perc_h = sum_h/sum(sum_h, na.rm = TRUE))

# Average or median energy burden in each HOLC category
avg_burden_holc <- mi_doe_redlining %>%
  # removing rows with na values for category
  filter(!is.na(category)) %>%
  # grouping by category
  group_by(category) %>%
  # taking the weighted mean and median
  summarise(mean_burden = weighted.mean(burden, h_count, na.rm = TRUE),
            median_burden = weightedMedian(burden, h_count, na.rm = TRUE)) %>%
  #ungrouping from category
  ungroup()

# Average or median energy burden in each HOLC category & FPL
avg_burden_holc_fpl <- mi_doe_redlining %>%
  # removing rows with na values for category
  filter(!is.na(category)) %>%
  # grouping by category
  group_by(category, fpl) %>%
  # taking the weighted mean and median
  summarise(mean_burden = weighted.mean(burden, h_count, na.rm = TRUE),
            median_burden = weightedMedian(burden, h_count, na.rm = TRUE)) %>%
  #ungrouping from category
  ungroup()

# Average or median energy burden in each HOLC category & FPL
avg_burden_fpl <- mi_doe_redlining %>%
  # removing rows with na values for category
  filter(!is.na(fpl)) %>%
  # grouping by category
  group_by(fpl) %>%
  # taking the weighted mean and median
  summarise(mean_burden = weighted.mean(burden, h_count, na.rm = TRUE),
            median_burden = weightedMedian(burden, h_count, na.rm = TRUE)) %>%
  #ungrouping from category
  ungroup()

# for boxplot of energy burdens for each HOLC - HELP
mi_doe_redlining_no_na <- mi_doe_redlining %>%
  filter(!is.na(category) & !is.na(h_count) & !is.na(burden) & !is.na(fpl))

holc_fpl_whisker <- data.frame(
  fpl = rep(mi_doe_redlining_no_na$fpl, mi_doe_redlining_no_na$h_count),
  burden = rep(mi_doe_redlining_no_na$burden, mi_doe_redlining_no_na$h_count),
  category = rep(mi_doe_redlining_no_na$category, mi_doe_redlining_no_na$h_count)
)
  


# Average or median income in each HOLC category
avg_income_holc <- mi_doe_redlining %>%
  # removing rows with na values for category
  filter(!is.na(category)) %>%
  # grouping by category
  group_by(category) %>%
  # finding the weighted median income
  summarise(avg_income = weightedMedian(income, h_count)) %>%
  # ungrouping from category
  ungroup()

# Percent of households within FPL
household_fpl_holc <- mi_doe_redlining %>%
  # taking gi, category, fpl, and h_count columns
  select(gi, category, h_count, fpl) %>%
  # removing rows with na values for gi,cateogry, or h_count
  filter(!is.na(category) & !is.na(gi) & !is.na(h_count))%>% 
  # grouping by category and fpl
  group_by(category, fpl) %>%
  # sum to find # of houses for each category and fpl
  summarise(sum_h = sum(h_count, na.rm = TRUE)) %>%
  #trying to add column for percent of households
  mutate(perc_h = sum_h/sum(sum_h, na.rm = TRUE)) %>%
  # ungrouping from category
  ungroup()

