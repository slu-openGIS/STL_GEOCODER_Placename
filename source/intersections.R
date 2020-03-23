# Generate Intesection Dataframe
library(tigris)
library(sf)
library(dplyr)
library(magrittr)
options(tigris_use_cache = FALSE)

# Get Roads for St. Louis
stl_roads <- roads(29, 510, class = 'sf')
stl_interstate <- filter(stl_roads, RTTYP == 'I')
stl_roads <- filter(stl_roads, RTTYP != 'I')


# Find Intersections for Interstates
intersections <- st_intersection(stl_interstate, stl_roads)

# Remove Spaces From Names
intersections$FULLNAME %<>% gsub('- ', '-', .)


intersections <-
  transmute(intersections,
    name = paste0(FULLNAME, ' at ', FULLNAME.1)
  )

# Remove Non-Point Geometries (Lines Overlapping)
points <- sapply(intersections[['geometry']], function(x){
  if ("POINT" %in% class(x)){
    return(TRUE)
  }else{
    return(FALSE)
  }
})

intersections <- filter(intersections, points)

# If Mulitple Intersections, Return Center Point
intersections %>%
  group_by(name) %>%
  summarise() -> groups

coords <- sapply(groups[['geometry']], function(x){
    if ("MULTIPOINT" %in% class(x)){
      coords <- st_coordinates(x)
      center <- c(lon = mean(coords[,1]),
                  lat = mean(coords[,2]))
      return(center)
    }else{
      return(st_coordinates(x))
    }
})
coords <- t(coords)

# Join the Data and Project to SF
join <- cbind(groups, coords)

final <- st_drop_geometry(join) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# To Test View
#library(mapview)
#mapview(final, legend = FALSE)

# To Make the Exported CSV
join %<>%
  st_drop_geometry %>%
  transmute(
    id = row_number() * 2,
    name,
    address = name,
    lat,
    lon
  )

reverse_name <- function(name){
  splt <- strsplit(name, ' at ')
  rev <- sapply(splt, function(x){
    paste0(x[2], ' at ', x[1])
  })
  return(rev)
}

join2 <-
  join %>%
  mutate(name = reverse_name(name),
         id = id - 1
  )

# Add Suffixes to Join2
suffixes <- c('',' Northbound', ' Southbound', ' Eastbound', ' Westbound')

join2 %<>%
  rbind(.,.,.,.,.) %>%
  arrange(id)
join2$name %<>% paste0(suffixes)
join2 %<>% mutate(id = row_number() * 2)

# Reverse Names for Original Join Again (With Suffixes)
join <-
  join2 %>%
  mutate(name = reverse_name(name),
         id = id - 1
  )

# Join and Order
export <- rbind(join, join2)
export <- export[order(export$id),]

readr::write_csv(export, 'data/intersections.csv')

