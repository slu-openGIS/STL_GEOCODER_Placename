# combine intersections

# dependencies
library(dplyr)
library(tibble)
library(readr)

# load data
intersections <- read_csv("data/intersections.csv")
places <- read_csv("data/STL_GEOCODER_Placename.csv")

# prepare
intersections %>%
  mutate(id = id+90000) %>%
  rename(
    latitude = lat,
    longitude = lon
  ) -> intersections

# bind
places <- bind_rows(places, intersections)

# output
write_csv(places, path = "data/STL_GEOCODER_Placename.csv")
