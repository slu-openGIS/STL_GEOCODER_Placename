# Make Park Coordinate Database
library(sf)
library(dplyr)
library(magrittr)
library(ggmap)
library(readr)

# Load Park Data
parks <- st_read("data/parks/parks.shp", stringsAsFactors = FALSE) %>% transmute(name = TEXT_)

# Calculate centroid and reproject
parks %<>% st_centroid %>% st_transform(4326)

# Anticipate Edge Cases
parks$name[1]   <- "Father Filipiac Park"
parks$name[40]  <- "Jet Banks Park"
parks$name[86]  <- "Gregory Feeman Park"
parks$name[100] <- "River Des Peres Extension Park"

# Restrict Geo to STL
parks$name <- paste0(parks$name, " STL")

# register_google("xxxxxxxxxxxxx")

# Geocode with Google Maps
address <- vector("list", length(parks$name))
for (i in seq_along(parks$name)) {
  address[[i]] <- geocode(parks$name[i], output = "all")
}

parse <- vector("list", length(parks$name))
g_lon <- vector("list", length(parks$name))
g_lat <- vector("list", length(parks$name))
for (i in seq_along(parks$name)) {
  parse[[i]] <- address[[i]]$results[[1]]$formatted_address
  g_lon[[i]] <- address[[i]]$results[[1]]$geometry$location$lng
  g_lat[[i]] <- address[[i]]$results[[1]]$geometry$location$lat
}

# Add to Data.frame
parks$address = unlist(parse)

# Extract Centroid Coords to Vars
parks$c_lon <- st_coordinates(parks)[,1]
parks$c_lat <- st_coordinates(parks)[,2]

# Add Google Maps Lat/Lon to Data
parks$g_lon <- unlist(g_lon)
parks$g_lat <- unlist(g_lat)

# Remove STL from Names
parks$name <- gsub("\\sSTL", "", parks$name)

# Remove sf Geometry
st_geometry(parks) <- NULL

# save to csv
write_csv(parks, "data/parks.csv")
