library(adehabitatHR)
library(sp)
library(maptools)
library(rgeos)
library(stringr)
library(gpclib)
library(rgdal)

#-------------------------------------------------------------------------------------------------------
# BEFORE YOU START
# the format for the input data are: unique animal id, x, y
# data must be projected (i.e., not in lat/lon) before calculating home ranges. For small areas, UTM is a 
# good choice. For larger areas, choose an equal area projection.

#-------------------------------------------------------------------------------------------------------
# set global parameters


# enter your folder locations here. Note that each home range size (10%, 50%, 95%, 100%) 
# is output to its own folder (hr10, hr50, hr95, hr)but this can be modified. You need to create those
# folders before you run the loop.

#folder where you input file is located
dfolder<-"H:\\GIS_Data\\GE\\Analysis_homerange_2015\\data\\r"

#input data file
datafile <- "test.txt"

#folder containing output folders as noted above
folder<-"H:\\GIS_Data\\GE\\Analysis_homerange_2015\\locoh\\test\\"


#------------------------------------------------------------------------------------------------------
#initialize routine
setwd(dfolder)
getwd()

data<-read.csv(datafile, header=TRUE)

#set unique animal id. Change "serial" to your unique animal id
psy<-data$serial

#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
# RUN A LOCOH LOOP
#generate list of unique animal ids
psylist <- unique(psy)
psylist

# loop through list /testing /use to wrap function 

for (i in psylist) 
{
  print(i)
  data1 <- subset(data, psy == i)
  data1.xy <- data1[c("x","y")]
  coordinates(data1.xy) <- c("x","y")
  max.a <- max(spDists(data1.xy))
  try(hr <- LoCoH.a(data1.xy, a = max.a, unin = "m", unout = "km2", duplicates = "random", 
		amount = 0.1), silent=TRUE)
  try(writePolyShape(hr, (paste(folder, "hr\\hr",i, sep = "")), 
                     factor2char = TRUE, max_nchar=254), silent=TRUE)
  try(writePolyShape((subset(hr,hr$percent<=10,)), (paste(folder, "hr10\\hr10",i, sep = "")),
                     factor2char = TRUE, max_nchar=254), silent=TRUE)
  try(writePolyShape((subset(hr,hr$percent<=50,)), (paste(folder, "hr50\\hr50",i, sep = "")), 
                     factor2char = TRUE, max_nchar=254), silent=TRUE)
  try(writePolyShape((subset(hr, hr$percent <= 95,)), (paste(folder, "hr95\\hr95",i, sep = "")), 
                     factor2char = TRUE, max_nchar=254), silent=TRUE)
  try(rm(hr), silent=TRUE)
};

# the above code will occassionally fail. If that occurs, you will get a warning message:
# "1: In rm(hr) : object 'hr' not found".
# Unfortunately, it will not tell you which one failed!
# If locoh fails to create a home range, multiple max.a * 1.1, 1.2, 1.3... until it is successful. 

# The shapefiles that are output will have way more features than you need. For some reason, there 
# seems to be lots of duplicate features. I use AddFileName.tbx to add the shape file name, which contains
# the unique animal id, to the attribute table. You can download that toolbox here: 
# found here: https://catalog.data.gov/dataset/arcgis-tool-inserts-file-name-into-attribute-table
# Then I merge all of the shape files into a single feature and extract a single feature for each unique
# animal id. The percent home range is never exactly 95%, etc. except for the 100% home range. Therefore, I select
# the maximum for each animal id and export those. 

#--------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------
#Use this to run a single home range, setting i = to the unique animal id

#set max.a multiplier
mult<-1.1

#set animal id
i = "3354S2012"


print(i)
data1 <- subset(data, psy == i)
data1.xy <- data1[c("x","y")]
coordinates(data1.xy) <- c("x","y")
max.a <- mult*(max(spDists(data1.xy)))
try(hr <- LoCoH.a(data1.xy, a = max.a, unin = "m", unout = "km2", duplicates = "random", amount = 0.1))
try(writePolyShape(hr, (paste(folder, "hr\\hr",i, sep = "")), 
            factor2char = TRUE, max_nchar=254), silent=TRUE)
try(writePolyShape((subset(hr,hr$percent<=10,)), (paste(folder, "hr10\\hr10",i, sep = "")),
            factor2char = TRUE, max_nchar=254), silent=TRUE)
try(writePolyShape((subset(hr,hr$percent<=50,)), (paste(folder, "hr50\\hr50",i, sep = "")), 
            factor2char = TRUE, max_nchar=254), silent=TRUE)
try(writePolyShape((subset(hr, hr$percent <= 95,)), (paste(folder, "hr95\\hr95",i, sep = "")), 
            factor2char = TRUE, max_nchar=254), silent=TRUE)



