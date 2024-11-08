library(tidyr)
library(sf)
library(units)
library(dplyr)
library(ggplot2)

nlcdmap <- data.frame(
  nlcd = c(11,12,21,22,23,24,31,41,42,43,51,52,71,72,73,74,81,82,90,95),
  description = c("Open Water", "Perennial Ice", "Developed Open", "Developed Low", "Developed Medium", "Developed High",
                  "Barren", "Deciduous Forest", "Evergreen Forest", "Mixed Forest", "Dwarf Scrub", "Shrub/Scrub",
                  "Grassland", "Sedge", "Lichens", "Moss", "Pasture", "Cultivated Crops", "Woody Wetlands", "Emergent Herbaceous"),
  higher = c("Water", "Ice", "Open", "Developed","Developed", "Developed", "Open", "Forest", "Forest", "Forest", "Shrub", "Shrub",
             "Open", "Open", "Open", "Open", "Open", "Open", "Wetland", "Wetland")
)

goea <- read.csv("~/Documents/goea_al.csv")
goea <- goea[,c("ASY", "animalmo", "Age1", "Age2", "Alias", "ts", "SeasonYr", "Sex","Latitude","Longitude")]

load("/home/jess/Documents/work/output/goea_hr/data/akde_95.RData")
nc_df <- `st_geometry<-`(akde_cora_95, NULL)
df <- as.data.frame(nc_df)

###HOME RANGE STUFF
load("/home/jess/Documents/work/output/goea_hr/data/lulc_hr.RData")
lulc <- lulc[!duplicated(lulc$id),]
lulc_long <- lulc %>% pivot_longer(-c(id, count))
lulc_long$Age2 <- goea$Age2[match(lulc_long$id, goea$animalmo)]
lulc_long$Sex <- goea$Sex[match(lulc_long$id, goea$animalmo)]
lulc_long$NA_L2CODE <- df$NA_L2CODE[match(lulc_long$id, df$id)]
lulc_long$prop_cover <- lulc_long$value / lulc_long$count
names(lulc_long)[names(lulc_long)=="name"] <- "nlcd"
lulc_long <- merge(lulc_long, nlcdmap, by="nlcd") #this is now NLCD proportions per homerange
lulc_long <- lulc_long[!lulc_long$id %in% c("64_2013", "70_2015", "251_2015", "64_2015"),]

#home range summary stats
lulc_atomic <- lulc_long %>% group_by(id, higher) %>% summarise(prop_cover = sum(prop_cover))
lulc_atomic$Age2 <- goea$Age2[match(lulc_atomic$id, goea$animalmo)]
lulc_atomic$Sex <- goea$Sex[match(lulc_atomic$id, goea$animalmo)]
lulc_atomic$NA_L2CODE <- df$NA_L2CODE[match(lulc_atomic$id, df$id)]

lulc_summ <- lulc_atomic %>% group_by(Age2, Sex, NA_L2CODE, higher) %>% summarise(prop_cover = mean(prop_cover, na.rm = TRUE))
lulc_summ$NA_L2NAME <- df$NA_L2NAME[match(lulc_summ$NA_L2CODE, df$NA_L2CODE)]
write.csv(lulc_summ, "homerange_lulc_summary.csv")

#home range summaries at NLCD class level by groupings
lulc_nlcd_summ <- lulc_long %>% group_by(Age2, Sex, NA_L2CODE, nlcd) %>% summarise(prop_cover = mean(prop_cover, na.rm = TRUE))
lulc_nlcd_summ$Description <- nlcdmap$description[match(lulc_nlcd_summ$nlcd, nlcdmap$nlcd)]
lulc_nlcd_summ$NA_L2NAME <- df$NA_L2NAME[match(lulc_nlcd_summ$NA_L2CODE, df$NA_L2CODE)]
write.csv(lulc_nlcd_summ, "homerange_nlcd_metrics.csv")

lulc_nlcd_eco <- lulc_long %>% group_by(NA_L2CODE, nlcd) %>% summarise(prop_cover = mean(prop_cover, na.rm = TRUE))
lulc_nlcd_eco$NA_L2NAME <- df$NA_L2NAME[match(lulc_nlcd_eco$NA_L2CODE, df$NA_L2CODE)]
lulc_nlcd_eco$Description <- nlcdmap$description[match(lulc_nlcd_eco$nlcd, nlcdmap$nlcd)]

ggplot(lulc_nlcd_eco, aes(Description, prop_cover)) + xlab("NLCD Class") + ylab("Proportion(%) Home Range Cover") +
  geom_bar(stat = "identity") +
  facet_wrap(~NA_L2NAME) + coord_flip()

lulc_nlcd_age <- lulc_long %>% group_by(Age2, nlcd) %>% summarise(prop_cover = mean(prop_cover, na.rm = TRUE))
#lulc_nlcd_age$NA_L2NAME <- goea$NA_L2NAME[match(lulc_nlcd_eco$NA_L2CODE, goea$NA_L2CODE)]
lulc_nlcd_age$Description <- nlcdmap$description[match(lulc_nlcd_age$nlcd, nlcdmap$nlcd)]

ggplot(lulc_nlcd_age, aes(Description, prop_cover)) + xlab("NLCD Class") + ylab("Proportion(%) Home Range Cover") +
  geom_bar(stat = "identity") +
  facet_wrap(~factor(Age2, levels=c("J", "S", "A"))) + coord_flip()

lulc_nlcd_sex <- lulc_long %>% group_by(Sex, nlcd) %>% summarise(prop_cover = mean(prop_cover, na.rm = TRUE))
#lulc_nlcd_age$NA_L2NAME <- goea$NA_L2NAME[match(lulc_nlcd_eco$NA_L2CODE, goea$NA_L2CODE)]
lulc_nlcd_sex$Description <- nlcdmap$description[match(lulc_nlcd_sex$nlcd, nlcdmap$nlcd)]

ggplot(lulc_nlcd_sex, aes(Description, prop_cover)) + xlab("NLCD Class") + ylab("Proportion(%) Home Range Cover") +
  geom_bar(stat = "identity") +
  facet_wrap(~Sex) + coord_flip()

akde_cora_95 <- akde_cora_95[!akde_cora_95$id %in% c("64_2013", "70_2015", "251_2015", "64_2015"),]
akde_cora_95_demo <- akde_cora_95
akde_cora_95_demo$Age2 <- goea$Age2[match(akde_cora_95$id, goea$animalmo)]
akde_cora_95_demo$Sex <- goea$Sex[match(akde_cora_95$id, goea$animalmo)] #this is now home range geospatial object with demographic info 
#below is summarizing per home range metrics by groupings
birdwide3 <- akde_cora_95_demo %>% group_by(Age2, Sex, NA_L2CODE) %>% summarise(mean_area = mean(area), sd_area = sd(area),
                                                                           min_area = min(area), max_area = max(area))
birdwide3$NA_L2NAME <- df$NA_L2NAME[match(birdwide3$NA_L2CODE, df$NA_L2CODE)]
nc_df <- `st_geometry<-`(birdwide3, NULL)
write.csv(nc_df, "homerange_area_metrics.csv")
#akde_cora_95$NA_L2CODE <- goea$NA_L2CODE[match(akde_cora_95$id, goea$animalmo)]
akde_goea <- merge(akde_cora_95_demo, lulc)

lulcwide <- lulc_atomic %>% pivot_wider(id_cols=id, names_from=higher, values_from=prop_cover)
lulcwide$area <- akde_cora_95$area[match(lulcwide$id, akde_cora_95$id)]
units(lulcwide$area) = 'km2'
lulcwide$area <- as.numeric(lulcwide$area)
testmodel <- glm(area ~ Forest, data=lulcwide)
testmodel <- glm(area ~ Open, data=lulcwide)

lulcwide$NA_L2CODE <- df$NA_L2CODE[match(lulcwide$id, df$id)]
lulcwide$NA_L2NAME <- df$NA_L2NAME[match(lulcwide$NA_L2CODE, df$NA_L2CODE)]

ozark <- lulcwide[lulcwide$NA_L2CODE=="8.4",]
plains <- lulcwide[lulcwide$NA_L2CODE=="8.3",]
testmodel <- glm(area ~ Forest, data=ozark)
testmodel <- glm(area ~ Forest, data=plains)
testmodel <- glm(area ~ Open, data=ozark)
testmodel <- glm(area ~ Open, data=plains)

lulcwide$Age2 <- goea$Age2[match(lulcwide$id, goea$animalmo)]
#juv <- lulcwide[lulcwide$Age2=="J",]
s <- lulcwide[lulcwide$Age2=="S",]
a <- lulcwide[lulcwide$Age2=="A",]

#testmodel <- glm(area ~ Forest, data=juv)
testmodel <- glm(area ~ Forest, data=s)
testmodel <- glm(area ~ Forest, data=a)
testmodel <- glm(area ~ Open, data=s)
testmodel <- glm(area ~ Open, data=a)
