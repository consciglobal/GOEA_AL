library(terra)
library(exactextractr)
library(amt)
library(purrr)
library(raster)
library(dplyr)
load("/home/jess/Documents/work/output/goea_hr/goea_AL_season.RData")
str_name <-'/home/jess/Documents/work/data/CSG/NLCD2016_2024_10_23_17_39_37.tif' 
baserast <- rast(str_name)
#baserast <- terra::project(baserast, "epsg:4326", method="near")
#writeRaster(baserast, '/home/jess/Documents/work/data/CSG/NLCD2016_4326.tif')
posslm2 = possibly(.f = hr_isopleths, otherwise = "Error")
hr <- hr%>%
  mutate(isopleth = map(hr_akde, ~posslm2(.x)),
         akde = map(hr_akde, function(x) raster(x[[2]])))

good_iso <- sapply(hr$isopleth, is.data.frame)
akde_iso <- hr$isopleth[good_iso]
ids <- hr$id[good_iso]
ided <- Map(cbind, akde_iso, id = ids)
akde_cora <- bind_rows(ided)
akde_cora_95 <- akde_cora[akde_cora$what=="estimate",]

pixel <- bind_rows(exact_extract(baserast, akde_cora_95, function(value, coverage_fraction ) {table(value)}))
pixel <- cbind(id = hr$id, pixel)
save(pixel, file="NLCD_extracted.RData")