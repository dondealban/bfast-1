####BFAST single workup for manually defined areas

####specify the environment for: ndvi files, brick file outputs (to be saved since the memory cost is high), 
####and output files (for each of the chosen outputs from bfast)

dirNDVI <- "/media/chesterton/opdrive01/NDVI/119062_gcp_pass3"
dirBrick <- "/media/chesterton/opdrive01/BRICKS/119062_gcp_pass3"
dirOut <- "/media/chesterton/opdrive01/OUTPUT/119062_full_gcp_pass3"

####list paramenter metadata to be included in the commands, file names and in a separate text file

tileNo <- "119062" ####landsat tile number 
asThresh <- "100000" ####threshold for the areaSieve function
minVal <- "4000" ####minimum value threshold for NDVI returns used in the reclassification of the image
bfastMonStart <- "2008" ####start monitoring date
bfastPP <- "irregular" ####type of preprocessing to be applied ("irregular" or "16-days")
bfastMonEnd <- NULL ####optional end of monitoring period, ex: c(2010, 1)
#bfastFormula <-  ####formula for the regression model (default is "response ~ trend + harmonic")

####list ndvi files

fNDVI <- list.files(dirNDVI, pattern=glob2rx("*.gri"), recursive=F, full.names=T)
fNDVIa <- list.files(dirNDVI, pattern=glob2rx("*.gri"), recursive=F, full.names=F)
nameslist <- list()
freclass <- list()

####get names for bfastSpatial

for (i in 1:length(fNDVIa)){
  w <- substr(fNDVIa[i], 6, 21)
  nameslist[i] <- w
}

####find extent manually

plot(raster(fNDVI[25]))
extent1 <- drawExtent(show=TRUE, col="red")

#### perform areaSieve and make brick

setwd(dirBrick)
brickName <- paste(tileNo, "_", asThresh, "_", minVal, ".tif", sep="")

for (i in 1:length(fNDVI)){
  r <- raster(fNDVI[i])
  c <- crop(r, extent1, snap="in", dataType=NULL)
  rc <- reclassify(c, c(-Inf, as.integer(minVal),NA))
  a <- areaSieve(rc, thresh=as.integer(asThresh), keepZeroes=T, directions=8)
  freclass[i] <- a
}

brick1 <- brick(freclass)
names(brick1) <- nameslist
writeRaster(brick1, filename = brickName, format = "GTiff", bylayer=F, overwrite=T)


#### get names for each of the bfast outputs

setwd(dirOut)
bfastName <- paste("bfm_", bfastMonStart, "_", tileNo, "_", asThresh, "_", minVal, ".tif", sep="")
changeName <- paste("bfm_change_", bfastMonStart, "_", tileNo, "_", asThresh, "_", minVal, ".tif", sep="")
magName <- paste("bfm_mag_", bfastMonStart, "_", tileNo, "_", asThresh, "_", minVal, ".tif", sep="")
magChangeName <- paste("bfm_magCh_", bfastMonStart, "_", tileNo, "_", asThresh, "_", minVal, ".tif", sep="")
magThreshName <- paste("bfm_magTh_", bfastMonStart, "_", tileNo, "_", asThresh, "_", minVal, ".tif", sep="")

#### perform bfmSpatial on extent

unix.time(bfm <- bfmSpatial(brick1, dates=NULL, pptype=bfastPP, start=c(as.integer(bfastMonStart), 1), formula=response ~ trend + harmon, order=4, monend=bfastMonEnd, lag=NULL, slag=NULL, mc.cores=7))
writeRaster(bfm, filename=bfastName, format="GTiff", overwrite=T)

change <- bfmChange(bfm)
writeRaster(change, filename=changeName, format="GTiff", overwrite=T)

magn <- bfmMagn(bfm)
writeRaster(magn, filename=magName, format="GTiff", overwrite=T)

magn.change <- bfmMagn(bfm, change=change)
writeRaster(magn.change, filename=magChangeName, format="GTiff", overwrite=T)

magn.thresh <- bfmMagn(bfm, change=change, thresh=-0.05)
writeRaster(magn.thresh, filename=magThreshName, format="GTiff", overwrite=T)

plot(bfm)

bfmPixel(brick1, dates=NULL, start=c(as.integer(bfastMonStart), 1), monend = NULL, cell=NULL, f=1, min.thresh = NULL, sceneID=NULL, sensor=NULL, interactive=T, plot=T)
