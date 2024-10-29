library(amt)
library(raster)
library(sf)
library(dplyr)
library(purrr)
library(mapview)
load("/home/jess/Documents/work/output/goea_hr/goea_AL_season.RData")
posslm2 = possibly(.f = hr_isopleths, otherwise = "Error")
hr <- hr%>%
  mutate(isopleth = map(hr_akde, ~posslm2(.x)),
         akde = map(hr_akde, function(x) raster(x[[2]])))

random_pts <- function(x, y) {random_points(x, n = nrow(y), presence=y)}

#plan(multisession, workers=27)
#randompts <- lapply(seq(1:nrow(hr)), random_pts_list)
hr_problem <- hr[13,]
hr <- hr[-13,]

akde_cora <- bind_rows(hr$isopleth)
akde_cora_95 <- akde_cora[akde_cora$what=="estimate",]
akde_cora_95$id <- hr$id

random_pts_list <- function(x) {random_points(akde_cora_95[x,], n = nrow(hr$data[[x]]), presence=hr$data[[x]])}

mylist <- list()
for(x in seq(1:nrow(hr))) {
  print(x)
  mylist[[length(mylist) + 1]] <- random_pts_list(x)
}

hr$matchpts <- mylist
ided <- Map(cbind, hr$matchpts, id = hr$id)
randompts <- bind_rows(ided)
randoms <- st_as_sf(randompts, coords=c("x_", "y_"), crs=4326)
mapview(randoms)
#write.csv(randompts, "~/Documents/randompts.csv")
eco <- st_read("~/Downloads/us_eco_l3")
eco <- st_transform(eco, st_crs(hr$isopleth[[1]]))

akde_cora_95$year <- as.factor(as.integer(sapply(strsplit(akde_cora_95$id, "_"), "[[", 2)))
plot(akde_cora_95[7,][1])
sf_use_s2(FALSE)
akde_cora_95 <- st_join(akde_cora_95, eco, st_intersects)
#write_sf(akde_cora_95, "akde_cora_95.gpkg")