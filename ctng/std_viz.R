rm(list = ls())
setwd("~/Documents/GitHub/small-things/ctng")
library(tidyverse)
library(sf)
library(tigris)
std <- as.data.frame(read.table("STDs9614.txt", sep="\t", header=TRUE))

std_2014 <- std %>%  filter(Notes == "") %>% filter(Disease %in% c("Gonorrhea","Chlamydia")) %>% 
  transmute(Disease = Disease,
            state = State, 
            year = as.character(Year), #reverts back to numeric later, this was to save me a headache
            rate = Rate)

# WONDER data only contains state by state stats up till 2014. 
# This is from: https://www.cdc.gov/std/stats16/tables/3.htm, copy pasted into a csv
chlam <- read.csv("chlamydia.csv")
chlam <- chlam[complete.cases(chlam),] # the original csv had an artifact in it
chlam_rates <- chlam[,c(1, 10:11)] #only using the years that are lacking data due to differences in rounding
names(chlam_rates) <- substring(names(chlam_rates), 5)
names(chlam_rates)[1] <- "state"
chlam_rates <- gather(chlam_rates, "year", "rate", 2:3)
chlam_rates$Disease <- "Chlamydia"
chlam_rates$rate <- as.numeric(gsub(',', '', chlam_rates$rate))

std_clean <- union(chlam_rates, std_2014)

# same as above, source: https://www.cdc.gov/std/stats16/tables/14.htm
gon <- read.csv("gonorrhea.csv")
gon_rates <- gon[,c(1, 10:11)] #only using the years that are lacking data due to differences in rounding
names(gon_rates) <- substring(names(gon_rates), 6) 
names(gon_rates)[1] <- "state"
gon_rates <- gather(gon_rates, "year", "rate", 2:3)
gon_rates$Disease <- "Gonorrhea"
gon_rates$rate <- as.numeric(gsub(',', '', gon_rates$rate))

std_clean <- union(gon_rates, std_clean)
std_clean$year <- as.numeric(std_clean$year)

# Data is complete

ggplot(std_clean) + geom_line(aes(x= year, y= rate, color = Disease)) + facet_wrap(~ state) + 
  labs(x = "Year", y = "Rate per 100,000") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("ctng_states.png", width = 10, height = 8)
