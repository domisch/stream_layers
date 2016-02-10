
###---------------------------------------------------------------------------#
### Download and crop the freshwater variables to a user-specified extent
###---------------------------------------------------------------------------#

### Sami Domisch, November 2015

### Load, rename, crop and export the variables in R. For the netCDF-4 files the ncdf4 library is needed and depending on the operating system, it needs to be downloaded and installed "manually" (see below). See also other useful functions in the raster package, and additional code on http://www.earthenv.org/streams to snap points to the stream network, and extract the variables to the points.


### Install the libraries
if (!require("raster")) {  install.packages("raster", dependencies = TRUE) ; library(raster)}
if (!require("rgdal")) {  install.packages("rgdal", dependencies = TRUE) ; library(rgdal)}
if (!require("ncdf4")) {  install.packages("ncdf4", dependencies = TRUE) ; library(ncdf4)} # manual install below
if (!require("maps")) {  install.packages("maps", dependencies = TRUE) ; library(maps)}
if (!require("foreach")) {  install.packages("foreach", dependencies = TRUE) ; library(foreach)}
if (!require("doParallel")) {  install.packages("doParallel", dependencies = TRUE) ; library(doParallel)}

### Set working directory
dir <- "c:/freshwater_variables"
dir.create(dir)
setwd(dir)

### For Windows, download the "ncdf4" library and install locally. 
### Here is an example for Windows 64-bit:
download.file("http://cirrus.ucsd.edu/~pierce/ncdf/win64/ncdf4_1.12.zip", 
               paste(getwd(), "/ncdf4_1.12.zip",  sep=""))
install.packages(paste(getwd(), "/ncdf4_1.12.zip", sep=""), repos=NULL) 
library(raster); library(ncdf4); library(maps); library(foreach); library(doParallel)

### Example: Load all 12 land cover variables for "average percent upstream cover" into a raster brick, crop the brick to a smaller extent, write layers to disk and convert to a data.frame in parallel:

### Download the average landcover variables from EarthEnv
download.file("http://data.earthenv.org/streams/landcover_average.nc", 
               paste(getwd(), "landcover_average.nc", sep="/"), mode = "wb")
### Load the 12 layers into a raster brick
lc_avg <- brick("landcover_average.nc")
### Check the number of layers
nlayers(lc_avg)
### Check the metadata for units, scale factors etc.
nc_open("landcover_average.nc")
### Add layer names. See Table S1 or the ReadMe for the sequence of the single layers 
names(lc_avg) <- paste(c("lc_avg"), sprintf("%02d", seq(1:12)), sep="_")
### Extract e.g. the "Evergreen Broadleaf Trees"
lc02 <- lc_avg[["lc_avg_02"]]
### Plot the world and draw extent for cropping below 60°N latitude:
x11(10.3); map('world'); abline(h=60, lty=5, lwd=2, col="red"); text(-176, 64, "60°N", col="red")
### Crop to smaller extent e.g. by clicking the upper left and lower right corners 
(ext <- drawExtent())
### Alternatively set the extent by coordinates
# ext <- extent(c(5,8,30,35))

### Crop entire raster brick in parallel
### Make cluster object
cl <- makePSOCKcluster(detectCores()-2) # leave two cores for background processes
# cl <- makePSOCKcluster(1) # if old PC use only 1 core
registerDoParallel(cl) # register parallel backend
getDoParWorkers() # show number of workers
### Crop all layers in the brick and write the cropped layers to disk
lc_avg_crop <- foreach(i=names(lc_avg), lc = unstack(lc_avg) ,  .final=stack, .packages = c("raster", "ncdf4")) %dopar% {
  options(rasterNCDF4 = TRUE)
  tmp <- crop(lc, ext, snap="in")
  filename=paste0(i, ".tif")
  writeRaster(tmp, filename=filename, overwrite=TRUE) # write cropped raster files to disk
  }

### Check the layers in the new cropped stack
plot(lc_avg_crop)

### Convert raster stack into a dataframe
lc_avg_crop_df <- foreach(i=names(lc_avg_crop), lc = unstack(lc_avg_crop), .combine=cbind.data.frame, .packages = c("raster")) %dopar% {
  as.data.frame(lc_avg_crop[[i]], na.rm=T)
  }
### Check output
head(lc_avg_crop_df); summary(lc_avg_crop_df)
stopCluster(cl) # stop parallel backend
### Remove temporary raster-files on the hard disk
showTmpFiles()
removeTmpFiles()
