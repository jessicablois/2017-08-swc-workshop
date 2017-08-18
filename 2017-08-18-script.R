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
  
  

