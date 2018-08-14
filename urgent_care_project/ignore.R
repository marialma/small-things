# this was originally to fix some issues I thought were popping up because of the csv format. I thought I'd do more with it, but didn't.

setwd("~/Documents/GitHub/small-things/urgent_care_project")
hospitals <- read.csv("Hospitals.csv")
library(dplyr)
hops <- hospitals
hops$NAME <- gsub(",", ' ', hops$NAME)
write.csv(hops, file = "hosp.csv")
