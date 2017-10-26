dir1 <- "/media/chesterton/opdrive_2/REFLECTANCE/119062_hdf"
dir2 <- "/media/chesterton/opdrive_2/REFLECTANCE/119062_processing"
dir3 <- "/media/chesterton/opdrive_2/REFLECTANCE/119062_test"
dir4 <- "/media/chesterton/opdrive_2/REFLECTANCE/119062_bricks"
dir5 <- "/media/chesterton/opdrive_2/REFLECTANCE/119062_bricks/4500"
dir6 <- "/media/chesterton/opdrive_2/REFLECTANCE/119062_bricks/5000"
extDir <- "/media/chesterton/opdrive_2/Extents/Katingan"


f1 <- list.files(dir1, pattern=glob2rx("*.hdf"), recursive=F, full.names=T)
f2 <- list.files(dir2, pattern=glob2rx("*.gri"), recursive=F, full.names=T)
f2a <- list.files(dir2, pattern=glob2rx("*.gri"), recursive=F, full.names=F)
f3 <- list()
f4 <- list()
extList <- list.files(extDir, full.names=T, recursive=F)
nameslist <- c()

for (i in 1:length(f2a)){
  w <- substr(f2a[i], 6, 21)
  nameslist[i] <- w
}

setwd(dir4)

for (i in 1:length(extList)){
  e <- extent(raster(extList[i]))
  yminE <- ymin(e) - 10000000
  ymaxE <- ymax(e) - 10000000
  e <- extent(c(xmin(e), xmax(e), yminE, ymaxE))
  brickName <- paste("brick_", i, ".tif", sep="")
  for (i in 1:length(f2)){
    r <- raster(f2[i])
    x <- crop(r, e, snap="in", dataType=NULL)
    y <- areaSieve(x, thresh=5000, keepZeroes=T, directions=8)
    f3[i] <- y
  }
  brickExt <- brick(f3)
  names(brickExt) <- nameslist
  writeRaster(brickExt, filename = brickName, format = "GTiff", bylayer=F, overwrite=T)
}

setwd(dir6)
x1 <- list()

f5 <- list.files(dir4, pattern=glob2rx("*.tif"), full.names=T, recursive=F)

for (i in 1:length(f5)){
  
  brickName2 <- paste("brick_", i, "_5000.tif", sep="")
  bfastName <- paste("bfm05_", i, "_5000.tif", sep="")
  changeName <- paste("bfm05_change_", i, "_5000.tif", sep="")
  magName <- paste("bfm05_mag_", i, "_5000.tif", sep="")
  magChangeName <- paste("bfm05_magCh_", i, "_5000.tif", sep="")
  magThreshName <- paste("bfm05_magTh_", i, "_5000.tif", sep="")
  
  brickX <- brick(f5[i])
  for (i in 1:length(brickX[1])){
    reclassX <- reclassify(brickX[[i]], c(-Inf, 5000,NA))
    x1[i] <- reclassX
  }
  brickY <- brick(x1)
  names(brickY) <- nameslist
  brick5000 <- writeRaster(brickY, filename=brickName2, format="GTiff", bylayer=F, overwrite=T)
  names(brick5000) <- nameslist
  
  unix.time(bfm05_5000 <- bfmSpatial(brick5000, dates=NULL, pptype="irregular", start=c(2005, 1), monend=NULL, lag=NULL, slag=NULL, mc.cores=6))
  writeRaster(bfm05_5000, filename=bfastName, format="GTiff", overwrite=T)
  
  change <- bfmChange(bfm05_5000)
  writeRaster(change, filename=changeName, format="GTiff", overwrite=T)
  magn <- bfmMagn(bfm05_5000)
  writeRaster(magn, filename=magName, format="GTiff", overwrite=T)
  magn.change <- bfmMagn(bfm05_5000, change=change)
  writeRaster(magn.change, filename=magChangeName, format="GTiff", overwrite=T)
  magn.thresh <- bfmMagn(bfm05_5000, change=change, thresh=-0.05)
  writeRaster(magn.thresh, filename=magThreshName, format="GTiff", overwrite=T)
  
}
