# Intersections in St. Louis
library(tigris)
library(dplyr)
library(magrittr)

stl_roads <- roads(29, 510, class = 'sf')

# Divide interstates and everything else
interstates <- filter(stl_roads, RTTYP == "I")
stl_roads %<>% filter(is.na(RTTYP) | RTTYP != 'I')

# Find intersections
intersections <- st_intersection(interstates, stl_roads)

intersections %<>% transmute(interstate = FULLNAME,
                             road = FULLNAME.1)
