

###---------------------------------------------------------------------------------#
### Move points to closest freshwater pixel and extract raster values to each point
###---------------------------------------------------------------------------------#

### Sami Domisch, December 2015

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

###------------------------------------------------------#
### Download freshwater-specific variables from EarthEnv 
###------------------------------------------------------#
download.file("http://data.earthenv.org/streams/landcover_average.nc", 
               paste(getwd(), "landcover_average.nc", sep="/"), mode = "wb")

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
               paste(getwd(), "snap_points.zip", sep="/"), mode = "wb")

### Unzip files
unzip("snap_points.zip", exdir=getwd(), junkpaths=TRUE)

### Read the shape file
pts <- readShapePoints("points.shp", verbose=T, proj4string=CRS("+proj=longlat +datum=WGS84"))
### Change column names
names(pts) <- c("ID", "longitude", "latitude")


### OPTIONAL:
### Load points from table
pts <- read.dbf("points.dbf", as.is=FALSE) # direclty from the shape file (faster than .shp if many points)
pts <- read.csv("points.csv", header=TRUE) # from a .csv table
### Transform the points to a spatial object
coordinates(pts) <- pts[2:3] # assign coordinates
proj4string(pts) <- "+proj=longlat +ellps=WGS84" # set projection
### Change column names
names(pts) <- c("ID", "longitude", "latitude")



###--------------------------------------------------------------------------#
### Snap the points to the closest pixel based on a distance threshold in km
###--------------------------------------------------------------------------#

### Download the Java-Tool from phycoweb.net
download.file("http://www.phycoweb.net/software/rasterGIS/moveCoordinatesToClosestDataPixel103.jar", 
               paste(getwd(), "moveCoordinatesToClosestDataPixel103.jar", sep="/"), mode = "wb")

### Citation: 
### Verbruggen, H. (2012) RasterTools: moveCoordinatesToClosestDataPixel.jar version 1.03, available at http://www.phycoweb.net/software

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
rows_removed <- which(!pts$ID %in% pts_snapped$ID) # get those ID's that were not moved to the stream grids
pts_removed <- pts[rows_removed,] # subset the raw SpatialPointsDataFrame
### Export these removed points as a shape file
writeOGR(pts_removed, "points_removed.shp", driver="ESRI Shapefile", layer="points_removed.shp")


### Plot the raw vs. snapped points
x11()
plot(raster_mask, col='grey') # stream network
points(pts_snapped[c("longitude", "latitude")], pch=16, col='blue') # points that were retained
points(pts_removed, pch=16, col='red') # points that were removed


###------------------------------------------------------------------------------#
### Get raster values (e.g. landcover) for each point and export as shape file
###------------------------------------------------------------------------------#


### Extract the raster values of each layer in parallel and write these into a dataframe
cl <- makePSOCKcluster(detectCores()-2) # leave two cores
# cl <- makePSOCKcluster(1) # if old PC use only 1 core
registerDoParallel(cl) # register parallel backend
getDoParWorkers() # show number of workers
### Run in parallel
pts_extract <- foreach(i = iter(names(lc_avg)), .combine='cbind', .packages = c("raster", "ncdf4")) %dopar% {
  options(rasterNCDF4 = TRUE)
  extract(lc_avg[[i]], pts_snapped[c("longitude", "latitude")], df=T)[2] 
  }
stopCluster(cl)


### Merge the extracted values to the (new) coordinate table
pts_extract <- cbind(pts_snapped, pts_extract)

### Export as table
write.csv(pts_extract, "points_extract.csv", row.names=F)

### Export as a ESRI shapefile for ArcGIS etc..
pts_extract_sp <- SpatialPointsDataFrame(coords=pts_extract[c("longitude", "latitude")], data=pts_extract)
proj4string(pts_extract_sp) <- "+proj=longlat +ellps=WGS84"
writePointsShape(pts_extract_sp, "points_extract_sp")
cat(showWKT(proj4string(pts_extract_sp)),file="points_extract_sp.prj") 
### Alternative but does not allow to overwrite existing shapefiles
# writeOGR(points_extract_sp, "points_extract_sp.shp", driver="ESRI Shapefile", layer="points_extract_sp.shp")


