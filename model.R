nlcdrando <- read.csv("~/Downloads/nlcd_randompts (1).csv")
nlcdrando$.geo <- NULL 
topo1 <- read.csv("~/Downloads/topo_randompts.csv")
topo1$.geo <- NULL 
topo1 <- topo1[,c("rowid", "elevation", "orograph", "slope", "aspect")]
topo2 <- read.csv("~/Downloads/topo_randompts2.csv")
topo2$.geo <- NULL 
topo2 <- topo2[,c("rowid", "eastness", "northness")]

covars <- merge(nlcdrando, topo1, by="rowid")
covars <- merge(covars, topo2, by="rowid")