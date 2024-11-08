library(ggplot2) # for plotting
library(dplyr)
library(zoo)
library(amt) # for movement and overlap
library(sf)
library(furrr)
library(purrr)
library(ctmm)
library(raster)
library(terra)
str_name <-'/home/jess/Documents/work/data/CSG/goea_hr/blank.tif' 
#baserast <- rast(str_name)

#values(baserast) <- NA
#baserast <- terra::project(baserast, "epsg:4326")

goea <- read.csv("~/Documents/work/data/CSG/goea_hr/EasternGOEA_winter_20240718/eGEw.txt")
meta <- read.csv("~/Documents/work/data/CSG/goea_hr/EasternGOEA_winter_20240718/MetaData_20240718.csv")

#goea <- goea[goea$Longitude < -70,]
al <- c(45,59,64,70,73,250,251,273,573,574,575,652,653, 249, 651)
tn <- meta$Animal_ID[meta$Organization=="TN"]
#rem <- c(935,937)
goea <- goea[goea$Animal_ID %in% al,]

test <- read.csv("~/Downloads/forHR_20241107.csv")
asy <- read.csv("~/Downloads/missing_hr_asy.csv")
names(test)[names(test)=="F_Animal_ID"] <- "Animal_ID"
names(test)[names(test)=="Date_"] <- "Date"
names(test)[names(test)=="Time_"] <- "Time"
test <- merge(test, asy)
test <- test[,names(test) %in% names(goea)]
test$fot <- NA
test$Age1 <- NA
goea <- rbind(goea, test)
goea <- goea[!is.na(goea$Fix),]
goea <- goea[goea$Fix == 3,]

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

group_lenth <- aggregate(x= group_len$Date,
                         # Specify group indicator
                         by = list(group_len$Bird),      
                         # Specify function (i.e. mean)
                         FUN = length)
names(group_lenth) <- c("Bird", "Days")
birdskeep <- group_lenth$Bird[which(group_lenth$Days >= 30)]
#JESS HARDCODE HERE FOR LAST MIN ANALYSIS ADDITION
#birdskeep <- c(249, 651)
goea <- goea[goea$Animal_ID %in% birdskeep,]

goea_filter <- goea %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  mutate(ts = as.POSIXct(Date_Time, format="%m/%d/%Y %H:%M:%S", tz="GMT"))

goea_filter <- goea_filter[with(goea_filter, order(Animal_ID, ts)),] #getting rid of first day of data
goea_date <- goea_filter[!duplicated(goea_filter$Animal_ID),]
goea_filter <- goea_filter[!goea_filter$animalday %in% goea_date$animalday,]
#write.csv(goea_filter, "~/Documents/goea_al.csv")
#goea_filter <- goea_filter[-which(goea_filter$Animal_ID==937 & goea_filter$ts < as.POSIXct("2024-03-08 17:11:14", tz="UTC")),]
#goea_filter <- goea_filter[-which(goea_filter$Animal_ID==935 & goea_filter$ts < as.POSIXct("2024-03-29 19:19:11", tz="UTC")),]
#goea_filter <- goea_filter[-which(goea_filter$Animal_ID==936 & goea_filter$ts < as.POSIXct("2024-04-01 14:06:26", tz="UTC")),]

group_len <- aggregate(x= goea_filter$Latitude,
                       # Specify group indicator
                       by = list(goea_filter$Animal_ID),      
                       # Specify function (i.e. mean)
                       FUN = length)
names(group_len) <- c("Bird", "Records")

turtles_track <- goea_filter %>%
  make_track(Longitude, Latitude, ts, id = animalmo, HDOP=HDOP, VDOP=VDOP, crs = 4326) %>%
  # Use nest() to allow us to deal with multiple animals (5 in sample set)
  nest(data = -"id") %>%
  arrange(id)

posslm1 = possibly(.f = hr_akde, otherwise = "Error")
#posslm2 = possibly(.f = hr_isopleths, otherwise = "Error")

plan(multisession, workers=3)
startTime <- Sys.time()
hr <- turtles_track %>% 
  #mutate(hrmcp = map(data, function(x) {tryCatch({hr_mcp(x, levels = c(1.0))}, error= function(cond){st_sf(st_sfc())})})) %>%
  mutate(hr_akde = future_map(data, ~posslm1(.x, model=fit_ctmm(.x, "auto")))) #I think it has to be in its own mutate() call to run parallel
save(hr, file="goeahr_add.RData")
#hr <- hr%>%
# mutate(isopleth = map(hr_akde, ~posslm2(.x)))

#TOO RESOURCE INTENSIVE! What's going on here?
#hr <- hr %>% mutate(
#  hr_akde_error = future_map(data, ~posslm1(.x, fit_ctmm(.x, "auto", uere = 1.67))))

endTime <- Sys.time() 
print(endTime - startTime)

