library(rgee)
library(data.table)
library(ggplot2)
library(ggpattern)
library(units)
library(dplyr)
library(purrr)
library(amt)
library(raster)
library(sf)
ee$Initialize(project='ee-gorzo')
srtm <- ee$Image("USGS/3DEP/1m")
landcover <- ee$Image(srtm$select("elevation"))

goea <- read.csv("~/Documents/work/data/CSG/goea_hr/EasternGOEA_winter_20240718/eGEw.txt")
goea <- goea[!is.na(goea$Fix),]
goea <- goea[goea$Fix == 3,]
#goea <- goea[goea$Longitude < -70,]
al <- c(45,59,64,70,73,250,251,273,573,574,575,652,653)
#tn <- meta$Animal_ID[meta$Organization=="TN"]
#rem <- c(935,937)
goea <- goea[goea$Animal_ID %in% al,]
goea$animalmo <- paste(goea$Animal_ID, goea$SeasonYr, sep="_")
goea <- goea[goea$Month_LocalTime %in% c(12,1,2),]

#check to see number of records per day per bird
goea$animalday <- paste(goea$Animal_ID, goea$Date)
table(goea$animalday)
group_len <- aggregate(x= goea$Latitude,
                       # Specify group indicator
                       by = list(goea$Date, goea$Animal_ID),      
                       # Specify function (i.e. mean)
                       FUN = length)
names(group_len) <- c("Date", "Bird", "Records")

goea_filter <- goea %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  mutate(ts = as.POSIXct(Date_Time, format="%m/%d/%Y %H:%M:%S", tz="GMT"))

goea_filter <- goea_filter[with(goea_filter, order(Animal_ID, ts)),] #getting rid of first day of data
goea_date <- goea_filter[!duplicated(goea_filter$Animal_ID),]
goea_filter <- goea_filter[!goea_filter$animalday %in% goea_date$animalday,]
df <- st_as_sf(x = goea_filter,                         
               coords = c("Longitude", "Latitude"),
               crs = 4326)
