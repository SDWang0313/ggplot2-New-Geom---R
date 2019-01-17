library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

setwd("C:/Users/sheng/Google Drive/3. Education/1. Autodidact/Coursera/Building Data Visualization Tools - Johns Hopkins")

#Copying the basic data import code from instruction
ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")

ext_tracks <- read_fwf("ebtrk_atlc_1988_2015.txt", 
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")

#Data cleaning
selected_tracks <- ext_tracks %>%
  filter(storm_name=="IKE", year==2008, month=="09", day=="13", hour=="12") %>%
  mutate(storm_name_true = paste(storm_name, "-", year, sep = "")) %>%
  select(storm_name_true, year, month, day, hour, latitude, longitude
         , paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_")
         , paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_")
         , paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"))

selected_tracks <- selected_tracks %>%
  unite(datetime_true, year, month, day, hour) %>%
  mutate(datetime_true = ymd_h(datetime_true)) %>%
  gather(key = key, value = dir_dist, -storm_name_true, -datetime_true, -latitude, -longitude)

selected_tracks <- selected_tracks %>%
  separate(key, c("to_del","wind", "dir")) %>%
  spread(dir, dir_dist) %>%
  rename("storm_id" = storm_name_true, "date" = datetime_true)

selected_tracks$to_del <- NULL
selected_tracks$longitude <- selected_tracks$longitude * -1 #Somehow longitude value needs to be inversed to show the actual value