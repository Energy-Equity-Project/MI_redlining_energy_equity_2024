# Libraries---------------------------------------------------------------------
library(tidyverse)
library(sf)
library(geojsonsf)
library(matrixStats)
# source("redliningp1.R")
source("data_summary.R")

# Data Visualizations-----------------------------------------------------------

# Number of households in each HOLC category
ggplot(households_holc, aes(x=category, y=sum_h)) + 
  #making a bar chart
  geom_bar(stat = "identity") +
  #setting x label to "HOLC Category"
  xlab("HOLC Category") +
  #setting y label to "Number of Households"
  ylab("Number of Households") +
  theme_bw()
ggsave("outputs/num_households_holc.png")

# Number of households in each FPL category
ggplot(households_fpl, aes(x=fpl, y=sum_h)) + 
  #making a bar chart
  geom_bar(stat = "identity") +
  #setting x label to "HOLC Category"
  xlab("FPL") +
  #setting y label to "Number of Households"
  ylab("Number of Households") +
  theme_bw()
ggsave("outputs/num_households_fpl.png")


# Average or median energy burden in each HOLC category
avg_burden_holc_long <- avg_burden_holc %>%
  #making the dataframe tall 
  pivot_longer(cols = c("mean_burden", "median_burden"),
               names_to = "avg_type",
               values_to = "avg_val"
  )

ggplot(avg_burden_holc_long, aes(x = category, y = avg_val, fill = avg_type)) +
  #creates side-by-side bar chart
  geom_col(position = "dodge") +
  #sets the color palette from brewer package
  scale_fill_brewer(palette = "Paired") +
  #labels x as "HOLC Category"
  xlab("HOLC Category") +
  #labels y as "Average Energy Burden"
  ylab("Average Energy Burden") +
  theme_bw()
ggsave("outputs/avg_burden_holc.png")


# Percent of households within FPL
ggplot(household_fpl_holc, aes(fill=fpl, y=perc_h, x=category)) + 
  #making stacked bar chart
  geom_bar(position="stack", stat="identity") +
  #setting colors
  scale_fill_brewer(palette = "RdYlBu") +
  # labeling x axis "HOLC Category"
  xlab("HOLC Category") +
  # labeling y axis "% Households in FPL"
  ylab("Percent of Households in each FPL") +
  theme_bw()
ggsave("outputs/perc_holc_fpl_stack.png")

ggplot(household_fpl_holc, aes(fill=fpl, y=perc_h, x=category)) + 
  #making stacked bar chart
  geom_bar(position="dodge", stat="identity") +
  #setting colors
  scale_fill_brewer(palette = "RdYlBu") +
  # labeling x axis "HOLC Category"
  xlab("HOLC Category") +
  # labeling y axis "% Households in FPL"
  ylab("Percent of Households within HOLC category in each FPL") +
  theme_bw()
ggsave("outputs/perc_holc_fpl_side.png")

# create a boxplot of the energy burdens for each HOLC
holc_fpl_whisker %>%
  ggplot(aes(x = category, y = burden, fill = fpl)) +
  geom_boxplot(outlier.shape = NA) +
  ylim(c(0,65)) + 
  xlab("HOLC Category") +
  ylab("Median Energy Burden") + 
  #setting colors
  scale_fill_brewer(palette = "RdYlBu") +
  theme_bw()
ggsave("outputs/burden_holc_boxplot1.png")

holc_fpl_whisker %>%
  ggplot(aes(x = fpl, y = burden, fill = category)) +
  geom_boxplot(outlier.shape = NA) +
  ylim(c(0,65)) + 
  xlab("FPL") +
  ylab("Median Energy Burden") +
  #setting colors
  scale_fill_brewer(palette = "RdYlBu") +
  theme_bw()
ggsave("outputs/burden_holc_boxplot2.png")


# weighted median energy burden by fpl class and HOLC category 
ggplot(avg_burden_holc_fpl, aes(fill=fpl, y=median_burden, x=category)) + 
  #making stacked bar chart
  geom_bar(position="dodge", stat="identity") +
  #setting colors
  scale_fill_brewer(palette = "RdYlBu") +
  # labeling x axis "HOLC Category"
  xlab("HOLC Category") +
  # labeling y axis "Median Burden"
  ylab("Median Burden") +
  theme_bw()
ggsave("outputs/median_burden_fpl_holc_bar.png")


# weighted median energy burden by fpl class
ggplot(avg_burden_fpl, aes(fill=fpl, y=median_burden, x=fpl)) + 
  #making stacked bar chart
  geom_bar(stat = "identity") +
  #setting colors
  scale_fill_brewer(palette = "RdYlBu") +
  # labeling x axis "FPL"
  xlab("FPL") +
  # labeling y axis "Median Burden"
  ylab("Median Burden") + 
  geom_hline(yintercept = 6) +
  theme_bw()
ggsave("outputs/median_burden_fpl_bar.png")


