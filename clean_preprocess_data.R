# Libraries---------------------------------------------------------------------
library(tidyverse)
library(sf)
library(geojsonsf)
library(matrixStats)

# Initial data reading----------------------------------------------------------

doe_lead_fpl_dir <- "data/DOE_LEAD/FPL/CSV"
# Note: always leave out last forward slash "/"
# Use this format: "data/DOE_LEAD/FPL/CSV"
# NOT this format: "data/DOE_LEAD/FPL/CSV/"

# Get all CSV file paths
mi_doe_files <- list.files(doe_lead_fpl_dir, full.names = TRUE)

# Create an empty dataframe "table" (aggregator)
mi_doe <- data.frame()

# Iterate through all MI DOE FPL data levels and append this data to the mi_doe dataframe
for (i in 1:length(mi_doe_files)) {
  
  # Current filename
  curr_filename <- mi_doe_files[i]
  print(paste("Reading", curr_filename))
  # Reading in current MI DOE data for specific FPL level
  curr_doe <- read.csv(curr_filename) %>%
    # turning is_dac column from string to numeric for comparison purposes later
    mutate(
      is_dac = case_when(
        is_dac == "False" ~ 0,
        is_dac == "True" ~ 1,
        TRUE ~ -1
      )
    )
  
  # Append current MI DOE data to aggregator MI DOE dataframe
  mi_doe <- mi_doe %>%
    bind_rows(curr_doe)
}

print("DONE")

# Counting the number of rows of data per unique census tract ID
mi_doe %>%
  group_by(gi) %>%
  tally() %>%
  arrange(desc(n))

# GIS---------------------------------------------------------------------------

# Redlining data available as GEOJSON
redlining <- geojson_sf("data/redlining_data/mappinginequality.json")

# Filtering redlining data to state of Michigan
mi_redlining <- redlining %>%
  filter(state == "MI") %>%
  # Correcting whitespace typo
  mutate(category = case_when(
    category == "Industrial and Commercial " ~ "Industrial and Commercial",
    TRUE ~ category
  ))

# Check which cities are included
sort(unique(mi_redlining$city)) # base R approach

mi_redlining %>% # tidyverse approach
  pull(city) %>%
  unique() %>%
  sort()

# Find all census tracts that are located within--------------------------------
# the neighborhoods outlined in the redlining data------------------------------

# Michigan US Census census tract designations
mi_2015 <- st_read("data/tl_2015_26_tract/tl_2015_26_tract.shp")
mi_2018 <- st_read("data/tl_2018_26_tract/tl_2018_26_tract.shp", promote_to_multi = TRUE)
mi_2020 <- st_read("data/tl_2020_26_tract/tl_2020_26_tract.shp")
mi_2022 <- st_read("data/tl_2022_26_tract/tl_2022_26_tract.shp")

# Check if there are any census ID number that appear in DOE but do not appear in US census

# Check if there are any census ID number that appear in US Census but do not appear in DOE census
unique(mi_2015$GEOID)[!(unique(mi_2015$GEOID) %in% unique(mi_doe$gi))] # 368 appear in US census but not in DOE
unique(mi_2018$GEOID)[!(unique(mi_2018$GEOID) %in% unique(mi_doe$gi))] # 368 appear in US census but not in DOE
unique(mi_2020$GEOID)[!(unique(mi_2020$GEOID) %in% unique(mi_doe$gi))] # 572 appear in US census but not in DOE
unique(mi_2022$GEOID)[!(unique(mi_2022$GEOID) %in% unique(mi_doe$gi))] # 572 appear in US census but not in DOE

# Overlapping redlining areas with census tract map
# Make sure both data sources are in the same projection
mi_redlining_tracts <- mi_redlining %>%
  # Reprojecting GIS layer
  st_transform(4269) %>%
  st_set_crs(4269) %>%
  # validating reprojected data
  st_make_valid() %>%
  st_intersection(mi_2018 %>%
                    # Reprojecting GIS layer
                    st_transform(4269) %>%
                    st_set_crs(4269) %>%
                    # validating reprojected data
                    st_make_valid())

# Good reason for calculating the overall area by category and census tract and
# designating census tract as category with majority area
mi_redlining_tracts %>%
  filter(GEOID == "26081000200") %>%
  ggplot() +
  geom_sf(aes(fill = category)) +
  theme_bw()

mi_redlining_tracts <- data.frame(
  geoid = mi_redlining_tracts$GEOID,
  category = mi_redlining_tracts$category,
  city = mi_redlining_tracts$city,
  area = st_area(mi_redlining_tracts)
) %>%
  mutate(area = as.numeric(area)) %>%
  group_by(geoid, category, city) %>%
  summarize(area = sum(area, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(geoid, city) %>%
  mutate(max_area = max(area)) %>%
  ungroup() %>%
  filter(area == max_area) %>%
  select(geoid, category, city) %>%
  distinct()

# There should only be 1 HOLC (redlining category) per census tract
mi_redlining_tracts %>%
  group_by(geoid) %>%
  tally() %>%
  arrange(desc(n))

# Map of redlining in Detroit
mi_redlining %>%
  filter(city == "Detroit") %>%
  # Correct ordering for categories
  mutate(category = factor(
    category,
    levels = c("Best", "Still Desirable", "Definitely Declining", "Hazardous", "Industrial and Commercial")
  )) %>%
  # filter out industrial and commercial zones
  filter(category != "Industrial and Commercial") %>%
  ggplot() +
  geom_sf(aes(fill = category)) +
  scale_fill_brewer(palette = "YlOrRd") +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    # legend.position = "bottom"
  ) +
  labs(title = "Detroit (Redlined areas)",
       fill = "HOLC (Redlining Category)")

#saves time by breaking it up
p <- mi_redlining %>%
  filter(city == "Detroit") %>%
  # Correct ordering for categories
  mutate(category = factor(
    category,
    levels = c("Best", "Still Desirable", "Definitely Declining", "Hazardous", "Industrial and Commercial")
  )) %>%
  # filter out industrial and commercial zones
  filter(category != "Industrial and Commercial") %>%
  ggplot() +
  geom_sf(aes(fill = category)) +
  scale_fill_brewer(palette = "YlOrRd") +
  theme_bw()

p + theme(
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  # legend.position = "bottom"
) +
  labs(title = "Detroit (Redlined areas)",
       fill = "HOLC (Redlining Category)")

# ggsave("outputs/plot1.png", width = 10, height = 8, units = "in")


# Map of redlining in Detroit (based on census tracts)
mi_2018 %>%
  left_join(
    mi_redlining_tracts,
    by = c("GEOID"="geoid")
  ) %>%
  filter(!is.na(category)) %>%
  filter(city == "Detroit") %>%
  # filter(GEOID == "26081000200") %>%
  # Correct ordering for categories
  mutate(category = factor(
    category,
    levels = c("Best", "Still Desirable", "Definitely Declining", "Hazardous", "Industrial and Commercial")
  )) %>%
  # filter out industrial and commercial zones
  filter(category != "Industrial and Commercial") %>%
  ggplot() +
  geom_sf(aes(fill = category)) +
  scale_fill_brewer(palette = "YlOrRd") +
  theme_bw() + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(title = "Detroit (Redlined census tracts)",
       fill = "HOLC (Redlining Category)")

# Adding redlining category to MI DOE LEAD data
mi_doe_redlining <- mi_doe %>%
  left_join(
    mi_redlining_tracts %>%
      mutate(geoid = as.numeric(geoid)),
    by = c("gi"="geoid")
  )

outputs_dir <- "outputs"
write.csv(mi_doe_redlining, file.path(outputs_dir, "mi_doe_redlining.csv"), row.names = FALSE)


# # Summary statistics on mi_doe_redlining dataframe
# # Note: gi = census tract number, h_count = number of households, burden = utility/energy burden, income = household income
# 
# #thinking abt number of household instead of %, household count vs census tract
# 
# mi_doe_redlining <- mi_doe_redlining %>%
#   mutate(category = factor(
#     category,
#     levels = c("Best", "Still Desirable", "Definitely Declining", "Hazardous", "Commercial", "Industrial", "Industrial and Commercial")
#   ))
# 
# # mi_doe_redlining %>% filter(!(category %in% c("Commercial", "Industrial", "Industrial and Commercial"))
# 
# # Number of census tracts in each HOLC category
# mi_doe_redlining %>%
#   group_by(category) %>%
#   count() %>%
#   ungroup()
# 
# census_holc <- mi_doe_redlining %>% 
#   #taking the gi and category columns into temp
#   select(gi, category) %>% 
#   #removing columns that have na values for category and gi
#   filter(!is.na(category) & !is.na(gi)) %>% 
#   #taking 1/5 of the columns, removing duplicates
#   distinct() %>% 
#   #grouping by category
#   group_by(category) %>% 
#   #counting the number of gi that are in that category
#   tally() %>% 
#   #ungrouping the data
#   ungroup() 
# 
# ggplot(census_holc, aes(x=category, y=n)) + 
#   #making a bar chart
#   geom_bar(stat = "identity") +
#   # setting x label to "HOLC Category"
#   xlab("HOLC Category") +
#   #setting y label to "Number of Census Tracts"
#   ylab("Number of Census Tracts")
# 
# 
# # Number of households in each HOLC category
# households_holc <- mi_doe_redlining %>%
#   # taking gi, category, and h_count columns
#   select(gi, category, h_count) %>%
#   # removing rows with na values for gi,cateogry, or h_count
#   filter(!is.na(category) & !is.na(gi) & !is.na(h_count)) %>% 
#   # grouping by category
#   group_by(category) %>%
#   # sum to find # of houses for each category
#   summarise(sum_h = sum(h_count, na.rm = TRUE)) %>%
#   # ungrouping from category
#   ungroup() %>%
#   # adding column of % of houses in each category of total
#   mutate(perc_h = sum_h/sum(sum_h, na.rm = TRUE))
# 
# ggplot(households_holc, aes(x=category, y=sum_h)) + 
#   #making a bar chart
#   geom_bar(stat = "identity") +
#   #setting x label to "HOLC Category"
#   xlab("HOLC Category") +
#   #setting y label to "Number of Households"
#   ylab("Number of Households")
# 
# #y axis as ____?? for the second one
# 
# # Average or median energy burden in each HOLC category
# avg_burden_holc <- mi_doe_redlining %>%
#   # removing rows with na values for category
#   filter(!is.na(category)) %>%
#   # grouping by category
#   group_by(category) %>%
#   # taking the weighted mean and median
#   summarise(mean_burden = weighted.mean(burden, h_count, na.rm = TRUE),
#             median_burden = weightedMedian(burden, h_count, na.rm = TRUE)) %>%
#   #ungrouping from category
#   ungroup()
# 
# avg_burden_holc_long <- avg_burden_holc %>%
#   #making the dataframe tall 
#   pivot_longer(cols = c("mean_burden", "median_burden"),
#                names_to = "avg_type",
#                values_to = "avg_val"
#   )
# 
# ggplot(avg_burden_holc_long, aes(x = category, y = avg_val, fill = avg_type)) +
#   #creates side-by-side bar chart
#   geom_col(position = "dodge") +
#   #sets the color palette from brewer package
#   scale_fill_brewer(palette = "Paired") +
#   #labels x as "HOLC Category"
#   xlab("HOLC Category") +
#   #labels y as "Average Energy Burden"
#   ylab("Average Energy Burden")
# 
# 
# # Average or median income in each HOLC category
# avg_income_holc <- mi_doe_redlining %>%
#   # removing rows with na values for category
#   filter(!is.na(category)) %>%
#   # grouping by category
#   group_by(category) %>%
#   # finding the weighted median income
#   summarise(avg_income = weightedMedian(income, h_count)) %>%
#   # ungrouping from category
#   ungroup()
# 
# # Percent of households within FPL
# household_fpl_holc <- mi_doe_redlining %>%
#   # taking gi, category, fpl, and h_count columns
#   select(gi, category, h_count, fpl) %>%
#   # removing rows with na values for gi,cateogry, or h_count
#   filter(!is.na(category) & !is.na(gi) & !is.na(h_count))%>% 
#   # grouping by category and fpl
#   group_by(category, fpl) %>%
#   # sum to find # of houses for each category and fpl
#   summarise(sum_h = sum(h_count, na.rm = TRUE)) %>%
#   #trying to add column for percent of households
#   mutate(perc_h = sum_h/sum(sum_h, na.rm = TRUE)) %>%
#   # ungrouping from category
#   ungroup()
# 
# ggplot(household_fpl_holc, aes(fill=fpl, y=perc_h, x=category)) + 
#   #making stacked bar chart
#   geom_bar(position="stack", stat="identity") +
#   #setting colors
#   scale_fill_brewer(palette = "RdYlBu") +
#   # labeling x axis "HOLC Category"
#   xlab("HOLC Category") +
#   # labeling y axis "% Households in FPL"
#   ylab("Percent of Households in each FPL")
# 
# ggplot(household_fpl_holc, aes(fill=fpl, y=perc_h, x=category)) + 
#   #making stacked bar chart
#   geom_bar(position="dodge", stat="identity") +
#   #setting colors
#   scale_fill_brewer(palette = "RdYlBu") +
#   # labeling x axis "HOLC Category"
#   xlab("HOLC Category") +
#   # labeling y axis "% Households in FPL"
#   ylab("Percent of Households in each FPL")
# 
# # but y-axis as _____ (first one)
# 
