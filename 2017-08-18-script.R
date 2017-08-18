# R script for SWC 2017-08-18

surveys <- read.csv("data/portal_data_joined.csv")


# Playing with pipes ----

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


# group_by and summarize

surveys %>%
  filter(sex == "M" | sex == "F",
         !is.na(weight)) %>%
  group_by(species_id, sex) %>%
  summarize(mean_weight = mean(weight),
            min_weight = min(weight)) 

## tally counts the total number of observations for the variables
surveys %>%
  group_by(sex,species_id) %>%
  tally

## Challenge

# 1. How many individuals were caught in each plot_type surveyed?

surveys %>%
  group_by(plot_type) %>%
  tally
  
  
# 2. Use group_by() and summarize() to find the mean, min, and max hindfoot
# length for each species (using species_id)

surveys %>%
  filter(!is.na(hindfoot_length)) %>%
  group_by(species) %>%
  summarize(mean_hfl = mean(hindfoot_length),
            min_hfl = min(hindfoot_length),
            max_hfl = max(hindfoot_length))

# ## 3. What was the heaviest animal measured in each year? Return
## the columns year, genus, species_id, and weight.

surveys %>%
  select(year, genus, species_id, weight) %>%
  group_by(year) %>%
  top_n(1, weight) %>%
  arrange(year)

surveys %>%
  filter(!is.na(weight)) %>%
  group_by(year) %>%
  filter(weight == max(weight)) %>%
  select(year, genus, species, weight) %>%
  arrange(year)
  
## 4. You saw above how to count the number of individuals of each sex using a
## combination of group_by() and tally(). How could you get the same result using
## group_by() and summarize()? Hint: see ?n.

surveys %>%
  group_by(sex) %>%
  summarise(n())

## Exporting data ----

# this works, but is a bit long and redundant
surveys_clean <- surveys %>%
  filter(species_id != "",) %>%   # remove missing species_id
  filter(!is.na(weight)) %>%
  filter(!is.na(hindfoot_length)) %>%
  filter(sex != "")

# this is a cleaner way to do it
surveys_clean <- surveys %>%
  filter(species_id != "",
         !is.na(weight),
         !is.na(hindfoot_length), 
         sex != "")

# find the common species
species_counts <- surveys_clean %>%
  group_by(species_id) %>%
  tally %>%
  filter(n >=50)
 
# only keep the common species in surveys_clean
surveys_common <- surveys_clean %>%
  filter(species_id %in% species_counts$species_id)

dir.create("data/clean")  
write.csv(surveys_common, file="data/clean/surveys_common.csv")


  
  
  