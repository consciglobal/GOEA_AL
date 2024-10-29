library(tidyr)
library(sf)
library(units)
topo <- read.csv("~/Downloads/topo_vars.csv")
topo$id <- paste(topo$ASY, topo$ts, sep="_")
topo <- topo[,c("id", "aspect", "elevation", "eastness", "northness", "orograph", "slope")]
#topo$.geo <- NULL
goea <- read.csv("~/Documents/goea_al.csv")
goea <- goea[,c("ASY", "animalmo", "Age1", "Age2", "Alias", "ts", "SeasonYr", "Sex","Latitude","Longitude")]
goea$id <- paste(goea$ASY, goea$ts, sep="_")

goea <- merge(goea, topo, by="id")
goea <- st_as_sf(goea, coords=c("Longitude", "Latitude"), crs=4326)
eco <- st_read("~/Downloads/us_eco_l3")
eco <- st_transform(eco, st_crs(goea))
sf_use_s2(FALSE)
goea <- st_join(goea, eco, st_intersects)

group_mean<- aggregate(x= goea[,c("aspect", "elevation", "eastness", "northness", "orograph", "slope")],
                       # Specify group indicator
                       by = list(goea$animalmo),      
                       # Specify function (i.e. mean)
                       FUN = mean)
group_mean <- as.data.frame(group_mean)
group_mean$geometry <- NULL
group_mean = gather(group_mean, variable, value, -Group.1)
group_mean$summ <- "mean"
group_max<- aggregate(x= goea[,c("aspect", "elevation", "eastness", "northness", "orograph", "slope")],
                       # Specify group indicator
                       by = list(goea$animalmo),      
                       # Specify function (i.e. mean)
                       FUN = max)
group_max <- as.data.frame(group_max)
group_max$geometry <- NULL
group_max = gather(group_max, variable, value, -Group.1)
group_max$summ <- "max"
group_min<- aggregate(x= goea[,c("aspect", "elevation", "eastness", "northness", "orograph", "slope")],
                       # Specify group indicator
                       by = list(goea$animalmo),      
                       # Specify function (i.e. mean)
                       FUN = min)
group_min <- as.data.frame(group_min)
group_min$geometry <- NULL
group_min = gather(group_min, variable, value, -Group.1)
group_min$summ <- "min"
group_sd<- aggregate(x= goea[,c("aspect", "elevation", "eastness", "northness", "orograph", "slope")],
                     # Specify group indicator
                     by = list(goea$animalmo),      
                     # Specify function (i.e. mean)
                     FUN = sd)
group_sd <- as.data.frame(group_sd)
group_sd$geometry <- NULL
group_sd = gather(group_sd, variable, value, -Group.1)
group_sd$summ <- "sd"

birdstats <- rbind(group_mean, group_min, group_max, group_sd)
birdstats$metric <- paste(birdstats$variable, birdstats$summ)
birdwide <- birdstats[,c("Group.1", "value", "metric")]
birdwide <- pivot_wider(birdwide, id_cols= "Group.1", names_from="metric")

ecoregions <- as.data.frame(goea[!duplicated(goea$animalmo),c(3:5,9,16:26)])
ecoregions$geometry <- NULL

birdwide <- merge(birdwide, ecoregions, by.x="Group.1", by.y="animalmo")
land <- read.csv("~/Downloads/topoland_vars.csv")
land$.geo <- NULL
land$id <- paste(land$ASY, land$ts, sep="_")
land <- land[,c("id", "nlcd13", "nlcd16", "nlcd19", "nlcd21")]
goea <- merge(goea, land, by="id")
goea13 <- goea[goea$SeasonYr %in% c(2013:2015),] 
goea16 <- goea[goea$SeasonYr %in% c(2016:2024),] 
goea13$nlcd <- goea13$nlcd13
goea16$nlcd <- goea16$nlcd16
goea <- rbind(goea13, goea16)
goea$nlcd13 <- NULL
goea$nlcd16 <- NULL
goea$nlcd19 <- NULL
goea$nlcd21 <- NULL
#saved from here
goea

test <- birdwide %>% group_by(Age2, Sex, NA_L2CODE) %>% summarise(
  aspect_mean = mean(`aspect mean`),
  elevation_mean = mean(`elevation mean`),
  eastness_mean = mean(`eastness mean`),
  northness_mean = mean(`northness mean`),
  orograph_mean = mean(`orograph mean`), 
  slope_mean = mean(`slope mean`),
  aspect_min =  mean(`aspect min`),
  elevation_min = mean(`elevation min`),
  eastness_min = mean(`eastness min`), 
  northness_min = mean(`northness min`), 
  orograph_min = mean(`orograph min`),
  slope_min = mean(`slope min`), 
  aspect_max = mean(`aspect max`),
  elevation_max = mean(`elevation max`),
  eastness_max = mean(`eastness max`),
  northness_max = mean(`northness max`),
  orograph_max = mean(`orograph max`),
  slope_max = mean(`slope max`),
  aspect_sd = mean(`aspect sd`),
  elevation_sd = mean(`elevation sd`),
  eastness_sd = mean(`eastness sd`),
  northness_sd = mean(`northness sd`),
  orograph_sd = mean(`orograph sd`),
  slope_sd = mean(`slope sd`)
)

#summary stats for topographic features 
summ <- test %>% pivot_longer(-c(Age2, Sex, NA_L2CODE))

summ_eagle <- goea  %>% group_by(animalmo) %>% summarise(n())
summ_eagle_lc <- goea  %>% group_by(animalmo, nlcd) %>% summarise(n())
summ_eagle_lc$count <- summ_eagle$`n()`[match(summ_eagle_lc$animalmo, summ_eagle$animalmo)]
summ_eagle_lc$Age2 <- goea$Age2[match(summ_eagle_lc$animalmo, goea$animalmo)]
summ_eagle_lc$Sex <- goea$Sex[match(summ_eagle_lc$animalmo, goea$animalmo)]
summ_eagle_lc$NA_L2CODE <- goea$NA_L2CODE[match(summ_eagle_lc$animalmo, goea$animalmo)]
summ_eagle_lc$prop_cover <- summ_eagle_lc$`n()` / summ_eagle_lc$count 

nlcdmap <- data.frame(
  nlcd = c(11,12,21,22,23,24,31,41,42,43,51,52,71,72,73,74,81,82,90,95),
  description = c("Open Water", "Perennial Ice", "Developed Open", "Developed Low", "Developed Medium", "Developed High",
                  "Barren", "Deciduous Forest", "Evergreen Forest", "Mixed Forest", "Dwarf Scrub", "Shrub/Scrub",
                  "Grassland", "Sedge", "Lichens", "Moss", "Pasture", "Cultivated Crops", "Woody Wetlands", "Emergent Herbaceous"),
  higher = c("Water", "Ice", "Open", "Developed","Developed", "Developed", "Open", "Forest", "Forest", "Forest", "Shrub", "Shrub",
             "Open", "Open", "Open", "Open", "Open", "Open", "Wetland", "Wetland")
)

summ_eagle_lc <- merge(summ_eagle_lc, nlcdmap, by="nlcd")
birdwide2 <- summ_eagle_lc[,c("nlcd", "animalmo", "prop_cover", "Age2", "Sex", "NA_L2CODE")]

#summary stats for land cover
birdwide2 <- birdwide2 %>% group_by(Age2, Sex, NA_L2CODE, nlcd) %>% summarise(prop_cover = mean(prop_cover))

load("/home/jess/Documents/work/output/goea_hr/data/lulc_hr.RData")
lulc <- lulc[!duplicated(lulc$id),]
lulc <- lulc %>% pivot_longer(-c(id, count))
lulc$Age2 <- goea$Age2[match(lulc$id, goea$animalmo)]
lulc$Sex <- goea$Sex[match(lulc$id, goea$animalmo)]
lulc$NA_L2CODE <- goea$NA_L2CODE[match(lulc$id, goea$animalmo)]
lulc$prop_cover <- lulc$value / lulc$count
names(lulc)[names(lulc)=="name"] <- "nlcd"
lulc <- merge(lulc, nlcdmap, by="nlcd")
#home range summary stats
lulc_atomic <- lulc %>% group_by(id, higher) %>% summarise(prop_cover = sum(prop_cover))
lulc_atomic$Age2 <- goea$Age2[match(lulc_atomic$id, goea$animalmo)]
lulc_atomic$Sex <- goea$Sex[match(lulc_atomic$id, goea$animalmo)]
lulc_atomic$NA_L2CODE <- goea$NA_L2CODE[match(lulc_atomic$id, goea$animalmo)]
lulc_summ <- lulc_atomic %>% group_by(Age2, Sex, NA_L2CODE, higher) %>% summarise(prop_cover = mean(prop_cover, na.rm = TRUE))

load("/home/jess/Documents/work/output/goea_hr/data/akde_95.RData")
akde_cora_95 <- akde_cora_95[!duplicated(akde_cora_95$id),]
akde_cora_95$Age2 <- goea$Age2[match(akde_cora_95$id, goea$animalmo)]
akde_cora_95$Sex <- goea$Sex[match(akde_cora_95$id, goea$animalmo)]
birdwide3 <- akde_cora_95 %>% group_by(Age2, Sex, NA_L2CODE) %>% summarise(mean_area = mean(area), sd_area = sd(area),
                                                                           min_area = min(area), max_area = max(area))
#akde_cora_95$NA_L2CODE <- goea$NA_L2CODE[match(akde_cora_95$id, goea$animalmo)]

lulcwide <- lulc_atomic %>% pivot_wider(id_cols=id, names_from=higher, values_from=prop_cover)
lulcwide$area <- akde_cora_95$area[match(lulcwide$id, akde_cora_95$id)]
units(lulcwide$area) = 'km2'
lulcwide$area <- as.numeric(lulcwide$area)
testmodel <- glm(area ~ Forest + Open, data=lulcwide)

