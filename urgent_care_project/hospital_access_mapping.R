setwd("~/Documents/GitHub/small-things/urgent_care_project")
rm(list=ls())
library(tidycensus)
library(tidyverse)
library(stringr)
options(tigris_use_cache = TRUE)
census_api_key("92c0f3f98616d1f0dc22c949794e68424bf7e625", install = TRUE)

dist <- read.csv('distance_2018.csv')
blockgroup_distances <- transmute(dist, 
                                  STATEFP = STATEFP, 
                                  COUNTYFP = COUNTYFP, 
                                  TRACTCE = TRACTCE, 
                                  POPULATION_x = POPULATION_x, 
                                  LATITUDE = LATITUDE, 
                                  LONGITUDE = LONGITUDE, 
                                  dist = dist)


blockgroup_distances$STATEFP <- str_pad(blockgroup_distances$STATEFP, 2, pad = "0")
blockgroup_distances$COUNTYFP <- str_pad(blockgroup_distances$COUNTYFP, 3, pad = "0")
blockgroup_distances$TRACTCE <- str_pad(blockgroup_distances$TRACTCE, 6, pad = "0")

blockgroup_distances <- blockgroup_distances %>%
  select(STATEFP, COUNTYFP, TRACTCE, POPULATION_x, LATITUDE, LONGITUDE, dist) %>%
  filter(STATEFP != 72, POPULATION_x > 0)


# uncomment to exclude alaska
#blockgroup_distances <- blockgroup_distances %>%
#  select(STATEFP, COUNTYFP, TRACTCE, POPULATION_x, LATITUDE, LONGITUDE, dist) %>%
#  filter(STATEFP != "02")

# 72 is Puerto Rico, which isn't in the Census dataset. 
# Also, all tracts with population = 0 aren't in the census dataset either.

bgdist <- transmute(blockgroup_distances,
                    GEOID = paste0(STATEFP,COUNTYFP,TRACTCE),
                    POPULATION_x = POPULATION_x, 
                    LATITUDE = LATITUDE, 
                    LONGITUDE = LONGITUDE, 
                    dist = dist)

#############
## MAPPING ##
#############


# Stolen from: https://walkerke.github.io/2017/05/tidycensus-every-tract/

library(sf)
us <- unique(fips_codes$state)[1:51]
totalpop_sf <- reduce(
  map(us, function(x) {
    #get_acs(geography = "tract", variables = "B01003_001", 
    #        state = x, geometry = TRUE)
    get_decennial(geography = "tract", variables = "P0010001",
                  year = 2010, state = x, geometry = TRUE)
  }), 
  rbind
)

str(totalpop_sf)

# setdiff(bgdist$GEOID, totalpop_sf$GEOID)
# Only one tract is missing in the two datasets now:  "12087980100", which has a population of 20 people. 
# I'm ok with just leaving that one out...
bgdist <- bgdist %>%
  select(GEOID, POPULATION_x, LATITUDE, LONGITUDE, dist) %>%
  filter(GEOID != 12087980100)
# Removing it so I don't have to worry about breaking any joins. 
# setdiff(bgdist$GEOID, totalpop_sf$GEOID) now returns character(0)

#removing all census tracts in the geo database that don't have any population 
geo_code <- totalpop_sf %>% 
  select(GEOID, NAME, value, geometry) %>%
  filter(value > 0)

#merge on GEOID
geo_dist <- merge(geo_code, bgdist, by="GEOID")
write.csv(geo_dist, "dist_with_mapping_coords.csv")

#cleanup
rm(blockgroup_distances, geo_code, dist, bgdist, totalpop_sf)



library(viridis)

distmap <- geo_dist %>%
  ggplot(aes(fill = dist, color = dist)) + 
  geom_sf() + 
  coord_sf(crs = 26911) +
  scale_fill_viridis(option = "magma") + 
  scale_color_viridis(option = "magma")



#############
# PLOTTING ONLY NO MAPPING

## Going to designate ~30km (35/2 miles) as an 'unreasonable' distance,
# since the federal designation for a CAH says that a CAH has to be at 
# least 35 miles by road away from the next nearest hospital. So, the
# midway point for that seems to be what the federal government considers
# to be an unreasonable distance away from a hospital.

thirty_km <- filter(blockgroup_distances, dist > 30)
forty_km <- filter(blockgroup_distances, dist > 40)
fifty_km <- filter(blockgroup_distances, dist > 50)
ggplot(thirty_km, aes(x=dist, fill=STATEFP)) + geom_histogram(bins=30) + 
  labs(title="States with tracts that are 100km away from the nearest hospital") +
  #scale_fill_discrete(labels=c("AK", "AZ","CA","FL","NE","NV","OR","SD","TX","UT")) +
  NULL
hundred_km <- filter(blockgroup_distances, dist > 100)

sum(thirty_km$POPULATION_x)


########## 
#JUST CA


CA <- get_decennial(geography = "tract", variables = "P0010001",
                        year = 2010, state = "CA", geometry = TRUE)
CA <- CA %>%
  select(GEOID, NAME, value, geometry) %>%
  filter(value > 0)

california <- blockgroup_distances %>%
  select(STATEFP, COUNTYFP, TRACTCE, POPULATION_x, LATITUDE, LONGITUDE, dist) %>%
  filter(STATEFP == "06", POPULATION_x > 0)

california <- transmute(california,
                    GEOID = paste0(STATEFP,COUNTYFP,TRACTCE),
                    POPULATION_x = POPULATION_x, 
                    LATITUDE = LATITUDE, 
                    LONGITUDE = LONGITUDE, 
                    dist = dist)
california_map <- merge(CA, california, by="GEOID")
distmap <- california_map %>%
  ggplot(aes(fill = dist, color = dist)) + 
  geom_sf() + 
  coord_sf(crs = 26911) +
  scale_fill_viridis(option = "magma", direction = -1) + 
  scale_color_viridis(option = "magma", direction = -1)


##### VT ####

VT <- get_decennial(geography = "tract", variables = "P0010001",
                    year = 2010, state = "VT", geometry = TRUE)
VT <- VT %>%
  select(GEOID, NAME, value, geometry) %>%
  filter(value > 0)

vermont <- blockgroup_distances %>%
  select(STATEFP, COUNTYFP, TRACTCE, POPULATION_x, LATITUDE, LONGITUDE, dist) %>%
  filter(STATEFP == "50", POPULATION_x > 0)

vermont <- transmute(vermont,
                        GEOID = paste0(STATEFP,COUNTYFP,TRACTCE),
                        POPULATION_x = POPULATION_x, 
                        LATITUDE = LATITUDE, 
                        LONGITUDE = LONGITUDE, 
                        dist = dist)
vt_map <- merge(VT, vermont, by="GEOID")
distmap <- vt_map %>%
  ggplot(aes(fill = dist, color = dist)) + 
  geom_sf() + 
  coord_sf(crs = 26911) +
  scale_fill_viridis(option = "magma", direction = -1) + 
  scale_color_viridis(option = "magma", direction = -1)
