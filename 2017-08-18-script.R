# R script for SWC 2017-08-18

surveys <- read.csv("data/portal_data_joined.csv")

install.packages("tidyverse")
library(tidyverse)

# select three columns from surveys: plot_id, species_id, weight
select(surveys, plot_id, species_id, weight)

# select certain rows, where year = 1995
filter(surveys, year == 1995)

# combine both of these steps into two
# PIPES! 
# This is a pipe: %>%

surveys_trimmed <- surveys %>%   # this is the input...ie, take surveys as the input
  filter(year == 1995) %>% # put surveys into filter
  select(plot_id, species_id, weight) # then take the output of the previous line and put it into select
  
# add a new column to surveys, weight_kg  
surveys %>%
  filter(!is.na(weight)) %>%
  mutate(weight_kg = weight / 1000,
         weight_kg2 = weight_kg *2) %>%
  head


# Challenge:

surveys %>%
  mutate(hfl0.5 = hindfoot_length / 2) %>%
  filter(!is.na(hfl0.5)) %>%
  select(species_id, hfl0.5) %>%
  tail
