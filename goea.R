library(ggplot2) # for plotting
library(dplyr)
library(zoo)
library(amt) # for movement and overlap
library(sf)
library(furrr)
library(purrr)
library(ctmm)
goea <- read.csv("~/Downloads/EasternGOEA_winter_20240718/eGEw.txt")
goea <- goea[!is.na(goea$Fix),]
goea <- goea[goea$Fix == 3,]
#goea <- goea[goea$Longitude < -70,]
al <- c(45,59,64,70,73,250,251,273,573,574,575,652,653)
cora <- goea[goea$Animal_ID %in% al,]
cora$animalmo <- paste(cora$Animal_ID, cora$Month_LocalTime, cora$Year_LocalTime, sep="_")

#check to see number of records per day per bird
cora$animalday <- paste(cora$Animal_ID, cora$Date)
table(cora$animalday)
group_len <- aggregate(x= cora$Latitude,
                       # Specify group indicator
                       by = list(cora$Date, cora$Animal_ID),      
                       # Specify function (i.e. mean)
                       FUN = length)
names(group_len) <- c("Date", "Bird", "Records")

turtles_track <- cora %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  mutate(ts = as.POSIXct(Date_Time, format="%m/%d/%Y %H:%M:%S", tz="GMT")) %>%
  make_track(Longitude, Latitude, ts, id = animalmo, HDOP=HDOP, VDOP=VDOP, crs = 4326) %>%
  # Use nest() to allow us to deal with multiple animals (5 in sample set)
  nest(data = -"id") %>%
  arrange(id)

posslm1 = possibly(.f = hr_akde, otherwise = "Error")
#posslm2 = possibly(.f = hr_isopleths, otherwise = "Error")

plan(multisession, workers=27)
startTime <- Sys.time()
hr <- turtles_track %>% 
  #mutate(hrmcp = map(data, function(x) {tryCatch({hr_mcp(x, levels = c(1.0))}, error= function(cond){st_sf(st_sfc())})})) %>%
  #this hr_mcp may not be suitable for parallel? it takes too long to run this way...
  mutate(hr_akde = future_map(data, ~posslm1(.x, fit_ctmm(.x, "auto")))) #I think it has to be in its own mutate() call to run parallel
save(hr, file="goeahr.RData")
#hr <- hr%>%
# mutate(isopleth = map(hr_akde, ~posslm2(.x)))

#TOO RESOURCE INTENSIVE! What's going on here?
#hr <- hr %>% mutate(
#  hr_akde_error = future_map(data, ~posslm1(.x, fit_ctmm(.x, "auto", uere = 1.67))))

endTime <- Sys.time() 
print(endTime - startTime)

