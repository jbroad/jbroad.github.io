# Maps
library(tigris)
options(tigris_use_cache = TRUE)
library(rsconnect)
library(shiny)
library(acs)
library(stringr) # to pad fips codes
library(leaflet)
library(sp)
library(sf)
library(htmlwidgets)
library(maps)
library(stringr)
library(XML)
library(ggplot2)
library(rgeos)
library(devtools)
setwd("~/Documents/GitHub/jbroad.github.io/shiny")

###### Setup

# Load polygon data
states <- states(cb = T) # Load data from tigris package
leg <- state_legislative_districts("Arizona", "lower") #Also from tigris
wrld <- st_as_sf(maps::map("world", fill = TRUE, plot = FALSE)) # from maps package

legreg2018 <- read.csv("legreg2018.csv") # District level data, 2018

# Pad the GEOID to match the polygon data
legreg2018$GEOID <- str_pad(as.character(legreg2018$GEOID), 5, side="left", pad="0")

# Merge the manipulated data with the polygon data
leg_merged<- geo_join(leg, legreg2018, "GEOID", "GEOID")

write.csv(leg_merged, file = "legreg2018_2.csv")



