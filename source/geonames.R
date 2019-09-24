library(dplyr)
library(readr)
library(testthat)

# create temp directory
tmp <- tempdir()

# download
download.file("http://download.geonames.org/export/dump/US.zip", file.path(tmp, "US.zip"))

# extract
unzip(file.path(tmp, "US.zip"), exdir = tmp)
geonames <- read_tsv(file.path(tmp, "US.txt"), col_names = FALSE,
                     col_types = cols(
                       X13 = col_character(),
                       X14 = col_character()
                     )) %>%
  filter(X11 == "MO" & X12 == "510") %>%
  select(-X10, -X13, -X14) %>%
  rename(
    geonameid = X1,
    name = X2,
    ascii_name = X3,
    alt_names = X4,
    latitude = X5,
    longitude = X6,
    feature_class = X7,
    feature_code = X8,
    country_code = X9,
    admin1 = X11,
    admin2 = X12,
    population = X15,
    elevation = X16,
    dem = X17,
    timezone = X18,
    date_modified = X19
  )

# make sure name and ascii_name are equivalent
expect_equal(all(geonames$name == geonames$ascii_name), TRUE)

# remove ascii_name
geonames <- select(geonames, -ascii_name)

# remove select observations
# geonames %>%
  # filter()

# split
geonames %>%
  transform(alt_names = strsplit(alt_names, ",")) %>%
  unnest(alt_names) %>%
  select(geonameid, name, alt_names, everything()) %>%
  as_tibble() -> geonames

# list of ids to include in data set
sub <- c("4379593")

# create table of addresses
add <- as_tibble(
  name = c("Calvary Cemetery")
  addrrecnum = c("10089598"),
  address = c("5239 W Florissant Ave")
)

# subset
geonames %>%
  filter(geonameid %in% sub) %>%
  select(geonameid, name, alt_names, latitutde, longitude)




