setwd("~/Documents/GitHub/small-things/urgent_care_project")
hospitals <- read.csv("Hospitals.csv")
closed_Hospitals <- read.csv("Closed_Hospitals.csv")
library(dplyr)
hops <- transmute(hospitals, 
                  "NAME" = NAME,
                  "TYPE" = TYPE,
                  "STATUS" = STATUS,
                  "LAT" = LATITUDE,
                  "LON" = LONGITUDE,
                  "STAT2" = X.1
                  )
hops$NAME <- gsub(",", ' ', hops$NAME)
write.csv(hops, file = "hosp.csv")


cenpop <- read.csv("pop_means.csv")



library(rgdal)
hosp <- readOGR(dsn="Hospitals", layer="Hospitals")
plot(hosp)
