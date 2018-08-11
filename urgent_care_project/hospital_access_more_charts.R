setwd("~/Documents/GitHub/small-things/urgent_care_project")
rm(list=ls())
library(tidycensus)
library(tidyverse)
library(stringr)
options(tigris_use_cache = TRUE)
#census_api_key("92c0f3f98616d1f0dc22c949794e68424bf7e625", install = TRUE)

dist <- read.csv('closed_er_next_closest_full.csv')
hosp_coords <- transmute(dist, 
                         STATEFP = STATEFP, 
                         COUNTYFP = COUNTYFP, 
                         TRACTCE = TRACTCE, 
                         POPULATION_x = POPULATION_x, 
                         LATITUDE = LATITUDE, 
                         LONGITUDE = LONGITUDE,
                         dist = dist,
                         new_dist = new_dist,
                         old_hosp_lat = LAT_x,
                         old_hosp_lon = LON_x,
                         new_hosp_lat = LAT_y,
                         new_hosp_lon = LON_y, 
                         distchange = distchange,
                         STATE = STATE, 
                         COUNTY = COUNTY)

hosp_coords$STATEFP <- str_pad(hosp_coords$STATEFP, 2, pad = "0")
hosp_coords$COUNTYFP <- str_pad(hosp_coords$COUNTYFP, 3, pad = "0")
hosp_coords$TRACTCE <- str_pad(hosp_coords$TRACTCE, 6, pad = "0")


hosp_coords <- transmute(hosp_coords,
                         GEOID = paste0(STATEFP,COUNTYFP,TRACTCE,"_"),
                         LATITUDE = LATITUDE,
                         LONGITUDE = LONGITUDE,
                         dist = dist,
                         new_dist= new_dist,
                         old_hosp_lat = old_hosp_lat,
                         old_hosp_lon = old_hosp_lon,
                         new_hosp_lat = new_hosp_lat,
                         new_hosp_lon = new_hosp_lon, 
                         distchange = distchange,
                         STATE = STATE, 
                         COUNTY = COUNTY,
                         POPULATION_x = POPULATION_x)

library(viridis)

#cleveland plots
cleveland <- transmute(hosp_coords,
                       GEOID = GEOID,
                       dist = dist,
                       new_dist= new_dist,
                       distchange =distchange,
                       POPULATION_x = POPULATION_x,
                       loc = paste(paste(str_to_title(COUNTY), "County", sep = " "), STATE, sep = ", "))
cleveland= cleveland[order(cleveland$new_dist),]
cleveland$GEOID = factor(cleveland$GEOID, levels = cleveland$GEOID[order(-cleveland$distchange)])
cleveland <- cleveland %>%
  select(GEOID, POPULATION_x, LATITUDE, LONGITUDE, dist) %>%
  filter(GEOID != 12087980100)


cl_label = mutate(cleveland, loc = loc, new_dist = new_dist)


cleveland_gathered<-gather(cleveland, key, value, -GEOID, -distchange, -loc, -POPULATION_x)
ggplot(cleveland_gathered, aes(value, GEOID)) +   geom_vline(xintercept=28, size = 1, color = "darkgrey") + 
  geom_line(aes(group=GEOID), color = "grey", size = 0.25) +
  geom_point(aes(color=key)) +
  theme(axis.title.y=element_blank(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  geom_text(data = cl_label, aes(label=loc, x = new_dist), hjust = -.5, color = "grey25", size = 1) +
  labs(x= "km from population center to nearest hospital") + scale_color_discrete(name = "", labels = c("Before Closures", "After Closures"))
NULL

# to do: re-sort this list by only closures that have happened since 2015. ?
ggsave("change_in_access.png", width = 6, height = 15, units = "in")



# Box and Whisker plot showing change in access
box_whisk <- transmute(hosp_coords, 
                       "Before Closures"= dist, 
                       "After Closures"= new_dist)
cohen.d(box_whisk$`Before Closures`, box_whisk$`After Closures`)
box_whisk <- gather(box_whisk, key, value)
ggplot(box_whisk, aes(x = key, y=value)) + 
  geom_boxplot(aes(fill = key), width = .25) + labs(y = "Distance in km") + 
  coord_flip() + labs(title = "Distance to Emergency Care: Before and After Closures") +
  theme(panel.border = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90"),
        panel.background = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") 

ggsave("box_whisk1.png", width = 6, height = 3)
# box and whisker plot for distance change
box_w_change <- transmute(hosp_coords, 
                          "Distance Change" = distchange)
box_w_change <- gather(box_w_change, key, value)
ggplot(box_w_change, aes(x = key, y=value)) + 
  geom_boxplot(aes(fill = key), width = .125) + labs(y = "Distance in km") + 
  coord_flip() + labs(title = "Increase in Emergency Care Travel Distance") +
  theme(panel.border = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90"),
        panel.background = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") 
ggsave("box_whisk2.png", width = 6, height = 2)


#### cleaner cleveland chart ----
# look only at communities who have had an distchange increase of greater than 10km 
# (which is the median ) 
cl <- cleveland %>%
  select(GEOID, dist, new_dist, distchange, POPULATION_x, loc) %>%
  filter(new_dist > 28 & distchange > 10, POPULATION_x >0)
cl= cl[order(cl$new_dist),]
cl$GEOID = factor(cl$GEOID, levels = cl$GEOID[order(-cl$distchange)])

cl_label = mutate(cl, loc = loc, new_dist = new_dist, pop = POPULATION_x)
cl_gathered<-gather(cl, key, value, -GEOID, -distchange, -loc, -POPULATION_x)

ggplot(cl_gathered, aes(value, GEOID)) +   geom_vline(xintercept=0, size = 1, color = "darkgrey") + 
  geom_line(aes(group=GEOID), color = "grey", size = 0.25) + xlim(0,150) + 
  geom_point(aes(color=key)) +
  theme(axis.title.y=element_blank(),  
        panel.grid.major.x = element_line(colour = "grey90"),
        plot.title = element_text(size = 18, hjust = 0.5),
        legend.title = element_text(size = 9),
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  geom_text(data = cl_label, aes(label=loc, x = new_dist), hjust = -.25, color = "grey50", size = 1.5) +
  labs(title = "Change in Emergency Care Access", x= "km from population center to nearest hospital") + scale_color_discrete(name = "", labels = c("Before Closures", "After Closures"))
NULL


ggsave("cleveland.png", width = 6, height = 5)


#Map for communities affected by closures

map <- transmute(hosp_coords,
                 GEOID = paste0(STATEFP,COUNTYFP),
                 POPULATION_x = POPULATION_x)
map <- map %>% group_by(GEOID) %>% summarize(pop = sum(POPULATION_x))

#this part stolen from https://walkerke.github.io/2017/05/tidycensus-every-tract/, but just for counties instead. 
# would have mapped tracts except R kept crashing. 

library(sf)
us <- unique(fips_codes$state)[1:51]
totalpop_sf <- reduce(
  map(us, function(x) {
    get_decennial(geography = "county", variables = "P0010001",
                  year = 2010, state = x, geometry = TRUE)
  }), 
  rbind
)

merged_map <- merge(totalpop_sf, map, by="GEOID", all.x=TRUE)
merged_map <- transmute(merged_map,
                        GEOID = GEOID,
                        pop = pop,
                        geometry = geometry)
merged_map2 <- merged_map %>%
  select(GEOID, pop, geometry) %>%
  filter(!str_detect(GEOID, "^02|^15")) #AK and HI kind of mess up my mapping a little

ggplot(merged_map2) + 
  geom_sf(aes(fill = pop), color = "grey75", lwd=0.1) + 
  coord_sf(datum = NA) + 
  theme_minimal() + 
  scale_fill_viridis(na.value = "white") + 
  labs(title = "Counties Heavily Affected by Hospital Closures", fill = "Population Size")
#write.csv(merged_map, "closures_map.csv")
ggsave("merged_map.png", width = 6, height = 3)
