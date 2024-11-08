boundbox <- st_bbox(c(xmin = min(turtles_track[1,2]$data[[1]]$x_), xmax = max(turtles_track[1,2]$data[[1]]$x_), ymax = max(turtles_track[1,2]$data[[1]]$y_), ymin = min(turtles_track[1,2]$data[[1]]$y_)), crs = st_crs(4326))
tester <- crop(baserast, boundbox)
test <- hr_akde(turtles_track[1,2]$data[[1]], fit_ctmm(turtles_track[1,2]$data[[1]], "auto"), trast = tester)

boundbox <- st_bbox(c(xmin = min(turtles_track[2,2]$data[[1]]$x_), xmax = max(turtles_track[2,2]$data[[1]]$x_), ymax = max(turtles_track[2,2]$data[[1]]$y_), ymin = min(turtles_track[2,2]$data[[1]]$y_)), crs = st_crs(4326))
tester <- crop(baserast, boundbox)
test2 <- hr_akde(turtles_track[2,2]$data[[1]], fit_ctmm(turtles_track[2,2]$data[[1]], "auto"), trast = tester)