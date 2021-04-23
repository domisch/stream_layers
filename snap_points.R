

###---------------------------------------------------------------------------------#
### Move points to closest freshwater pixel and extract raster values to each point
###---------------------------------------------------------------------------------#

### Sami Domisch, January 2017

### Install and load libraries
if (!require("raster")) {  install.packages("raster", dependencies = TRUE) ; library(raster)}
# if (!require("ncdf4")) {  install.packages("ncdf4", dependencies = TRUE) ; library(ncdf4)} # manual installation 
if (!require("rgdal")) {  install.packages("rgdal", dependencies = TRUE) ; library(rgdal)}
if (!require("sp")) {  install.packages("sp", dependencies = TRUE) ; library(sp)}
if (!require("foreign")) {  install.packages("foreign", dependencies = TRUE) ; library(foreign)}
if (!require("maptools")) {  install.packages("maptools", dependencies = TRUE) ; library(maptools)}
if (!require("foreach")) {  install.packages("foreach", dependencies = TRUE) ; library(foreach)}
if (!require("doParallel")) {  install.packages("doParallel", dependencies = TRUE) ; library(doParallel)}
if (!require("maps")) {  install.packages("maps", dependencies = TRUE) ; library(maps)}


### Set working directory
dir <- "C:/freshwater_variables"
dir.create(dir)
setwd(dir)

### Prepare function to merge dataframes with different column names, keep only those that match
### Original function obtained from https://amywhiteheadresearch.wordpress.com/2013/05/13/combining-dataframes-when-the-columns-dont-match/
rbind.match.columns <- function(input1, input2) {
    n.input1 <- ncol(input1)
    n.input2 <- ncol(input2)
 
    if (n.input2 < n.input1) {
        TF.names <- which(names(input2) %in% names(input1))
        column.names <- names(input2[, TF.names])
    } else {
        TF.names <- which(names(input1) %in% names(input2))
        column.names <- names(input1[, TF.names])
    }
 
    return(rbind(input1[, column.names], input2[, column.names]))
}


###------------------------------------------------------#
### Download freshwater-specific variables from EarthEnv 
###------------------------------------------------------#
download.file("http://data.earthenv.org/streams/landcover_average.nc", 
               paste0(dir, "/landcover_average.nc"), mode = "wb")

### Load layers and rename the single bands
lc_avg <- brick("landcover_average.nc")
names(lc_avg) <- paste(c("lc_avg"), sprintf("%02d", seq(1:12)), sep="_")
# slope <- brick("slope.nc")
# names(slope) <- paste(c("slope"), c("min", "max", "range", "avg"), sep="_")

### Extract one layer 
# lc01 <- lc_avg[["lc_avg_01"]]


###-------------------------------------------------------------------------#
### Load the points, either the entire shapefile or only the coordinates
###-------------------------------------------------------------------------#

### Load example points shape file. See the .shp or .csv files in the zip-folder on github:
download.file("http://github.com/domisch/stream_layers/raw/master/snap_points.zip", 
               paste0(dir, "/snap_points.zip"), mode = "wb")

### Unzip files
unzip("snap_points.zip", exdir=getwd(), junkpaths=TRUE)

### Read the shape file
pts <- readShapePoints("points.shp", verbose=T, proj4string=CRS("+proj=longlat +datum=WGS84"))
### Change column names
names(pts) <- c("ID", "longitude", "latitude")


### OPTIONAL:
### Load points from table
pts <- read.dbf("points.dbf", as.is=FALSE) # directly from the .dbf file (faster than .shp if many points)
pts <- read.csv("points.csv", header=TRUE) # from a .csv table
### Transform the points to a spatial object
coordinates(pts) <- pts[2:3] # assign coordinates
proj4string(pts) <- "+proj=longlat +ellps=WGS84" # set projection to WGS84
### Change column names
names(pts) <- c("ID", "longitude", "latitude")



###--------------------------------------------------------------------------#
### Snap the points to the closest pixel based on a distance threshold in km
###--------------------------------------------------------------------------#

### Download the Java-Tool from phycoweb.net
download.file("https://github.com/hverbruggen/RasterTools/blob/master/moveCoordinatesToClosestDataPixel104.jar", 
               paste0(dir, "/moveCoordinatesToClosestDataPixel103.jar"), mode = "wb")


### Citation: 
### Verbruggen H. (2017) RasterTools: moveCoordinatesToClosestDataPixel.jar version 1.04. https://github.com/hverbruggen/RasterTools

### Crop the base raster layer to a slightly larger extent that the points and save as .asc
### Run in smaller subsets if Java heap space error occurs (=RAM limitation due to large study area)
raster_mask <- crop(lc_avg[[1]], extent(pts)+0.5)
writeRaster(raster_mask, "raster_mask.asc", NAflag=-9999, overwrite=TRUE)
### Save the (raw) coordinates for snapping
write.csv(subset(as.data.frame(pts), select=c(ID,longitude,latitude)), "points_for_snap.csv", row.names=FALSE, quote=FALSE)

### Run Java tool: You may need to set the "path" variable in the system settings, 
### see https://www.java.com/en/download/help/path.xml

# system("cmd /c  java -version") # check if Java is installed.  
# system("cmd /c  java -jar moveCoordinatesToClosestDataPixel103.jar") # see options and flags

#    -i   input coordinates file (csv)
#    -r   raster used to determine which pixels have data (esri ascii format)
#    -o   output coordinates file (csv)
# 
# optional parameters
#    -md  maximum distance that new coordinates are allowed to be from original coordinates (in km)

###--- Snapping tolerance of 1 km ----
system("cmd /c  java -jar C:/freshwater_variables/moveCoordinatesToClosestDataPixel103.jar  -i  C:/freshwater_variables/points_for_snap.csv   -r  C:/freshwater_variables/raster_mask.asc    -o  C:/freshwater_variables/points_snapped.csv  -md 1")


### Reload the snapped coordinate file. Those points that fell outside the pixels were removed. 
### Use an ID column to keep track of these..
pts_snapped <- read.csv("points_snapped.csv", h=T)
head(pts_snapped) # contains old and new coordinates
### Remove the old coordinate columns
pts_snapped <- subset(pts_snapped, select=-c(old_longitude, old_latitude))


### Which points were removed?
"%ni%" <- Negate("%in%") # create a "not in" -function
rows_removed <- which(pts$ID %ni% pts_snapped$ID) # get those ID's that were not moved to the stream grids

### The Java-tool has a small bug: if the point is in the center of the grid cell, it may be 
### considered outside the distance tolerance. 
### To keep these points anyway:, substitute the snapped points in the original data, then extract 
### the values from the raster layers (see next step below). If a point does not fall on the grid cell, no value is 
### extracted and that point is not used anymore.

if (length(rows_removed)>0) { 
	pts_removed = pts[rows_removed,]   # subset the raw SpatialPointsDataFrame
	pts_removed$snapped=0              # helps subsetting later on
  pts_removed=as.data.frame(pts_removed)
	pts_snapped=rbind.match.columns(pts_snapped, pts_removed) # merge anyway due to bug in the snapping
}

### Create spatial objects
coordinates(pts_snapped)=c("longitude", "latitude")
proj4string(pts_snapped) <- CRS("+proj=longlat +datum=WGS84")
coordinates(pts_removed)=c("longitude", "latitude")
proj4string(pts_removed) <- CRS("+proj=longlat +datum=WGS84")

### Export the points as a shape file
writeOGR(pts_snapped, paste0(dir, "/pts_snapped.shp"), driver="ESRI Shapefile", layer="pts_snapped.shp")



# ### Plot the raw vs. snapped points
# ### Obsolete due to the bug..
# x11()
# plot(raster_mask, col='grey') # stream network
# points(pts_snapped[c("longitude", "latitude")], pch=16, col='blue') # points that were retained
# points(pts_removed, pch=16, col='red') # points that were removed



###------------------------------------------------------------------------------#
### Get raster values (e.g. landcover) for each point and export as shape file
###------------------------------------------------------------------------------#


### Extract the raster values of each layer in parallel and write these into a dataframe
cl <- makePSOCKcluster(detectCores()-2) # leave two cores
# cl <- makePSOCKcluster(1) # if old PC use only 1 core
registerDoParallel(cl) # register parallel backend
getDoParWorkers() # show number of workers

### Run in parallel, might take a while depending on the number of points
pts_extract <- foreach(lc = unstack(lc_avg), .combine='cbind', .packages = c("raster", "ncdf4")) %dopar% {
  options(rasterNCDF4 = TRUE)
  extract(lc, pts_snapped, df=T)[2] 
  }
stopCluster(cl)


### Merge the extracted values to the (new) coordinate table
pts_extract <- cbind(as.data.frame(pts_snapped), pts_extract)
# pts_extract <- cbind(coordinates(pts_snapped), pts_extract) # drops the ID


### Export as table
write.csv(pts_extract, "points_extract.csv", row.names=F)

### Export as a ESRI shapefile for ArcGIS etc..
pts_extract_sp <- SpatialPointsDataFrame(coords=pts_extract[c("longitude", "latitude")], data=pts_extract)
proj4string(pts_extract_sp) <- "+proj=longlat +ellps=WGS84"
writePointsShape(pts_extract_sp, "points_extract_sp")
cat(showWKT(proj4string(pts_extract_sp)),file="points_extract_sp.prj") 
### Alternative but does not allow to overwrite existing shapefiles
# writeOGR(points_extract_sp, "points_extract_sp.shp", driver="ESRI Shapefile", layer="points_extract_sp.shp")


