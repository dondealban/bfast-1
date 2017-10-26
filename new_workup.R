#new coding layout for processing

dir1 <- "/media/chesterton/opdrive_2/REFLECTANCE/119062_hdf"
dir2 <- "/media/chesterton/opdrive_2/REFLECTANCE/119062_processing"
dir3 <- "/media/chesterton/opdrive_2/REFLECTANCE/119062_test"

f1 <- list.files(dir1, pattern=glob2rx("*.hdf"), recursive=F, full.names=T)
f2 <- list.files(dir2, pattern=glob2rx("*.gri"), recursive=F, full.names=T)
f2a <- list.files(dir2, pattern=glob2rx("*.gri"), recursive=F, full.names=F)

#processLandsatBatch(x=dir1, pattern=glob2rx("*.hdf"), outdir=dir2, srdir=dir2, delete=F, vi="ndvi", mask="fmask", keep=0, overwrite=T, untar=F)

f3 <- list()
f4 <- list()
nameslist <- c()

#for (i in 1:length(f2)){
#  x <- raster(f2[i])
#  f3[i] <- x
#}

#plot(f3[[21]])
#e <- saveExtent()
#extent(c(727774, 763434, -279659, -255012))

#for (i in 1:length(f3)){
#  y <- crop(f3[[i]], e, snap="in", dataType=NULL)
#  z <- areaSieve(y, thresh=5000, keepZeroes=T, directions=8)
#  f4[i] <- z
#}

#brick1 <- brick(f4)
#writeRaster(brick1, filename = "brick1.tif", format = "GTiff", bylayer=F)


brick1 <- brick("/media/chesterton/opdrive_2/REFLECTANCE/119062_test/brick1.tif")
obs <- countObs(brick1, navalues = c(NA), sensor="all", as.perc = F)

writeRaster(obs, filemane = "obsCount.tif", format = "GTiff")


for (i in 1:length(f2a)){
  w <- substr(f2a[i], 6, 21)
  nameslist[i] <- w
}

names(brick1) <- nameslist

bfmP <- bfmPixel(brick1, start=c(2005, 1), monend=NULL, cell=NULL, f=1, min.thresh=NULL, sceneID=NULL, dates=NULL, sensor=NULL, interactive=TRUE, plot=TRUE)

cell1 <- bfmP$cell
thresh <- c(6000)

bfmp2 <- bfmPixel(brick1, start=c(2005, 1), monend=NULL, cell=cell1, f=1, min.thresh=c(6500), sceneID=NULL, dates=NULL, sensor=NULL, interactive=FALSE, plot=TRUE)

f6 <- list()
for (i in 1:length(brick1[1])){
  q <- reclassify(brick1[[i]], c(-Inf, 5000,NA))
  f6[i] <- q
}

brick5000 <- brick(f6)
brick5000 <- writeRaster(brick5000, filename="brick5000.tif", format="GTiff", overwrite=T)
names(brick5000) <- nameslist

#bfmp2 <- bfmPixel(brick5500, start=c(2005, 1), monend=NULL, cell=NULL, f=1, min.thresh=NULL, sceneID=NULL, dates=NULL, sensor=NULL, interactive=TRUE, plot=TRUE)


#unix.time(bfm05 <- bfmSpatial(brick1, dates=NULL, pptype="irregular", start=c(2005, 1), monend=NULL, lag=NULL, slag=NULL, mc.cores=6))
unix.time(bfm05_5000 <- bfmSpatial(brick5000, dates=NULL, pptype="irregular", start=c(2005, 1), monend=NULL, lag=NULL, slag=NULL, mc.cores=6))

#writeRaster(bfm05, filename="bfm05", format="GTiff", overwrite=T)
writeRaster(bfm05_5000, filename="bfm05_5000", format="GTiff", overwrite=T)

change <- bfmChange(bfm05_5000)
plot(change)
writeRaster(change, filename="change_5000", format="GTiff", overwrite=T)
magn <- bfmMagn(bfm05_5000)
plot(magn)
writeRaster(magn, filename="magn_5000", format="GTiff", overwrite=T)
magn.change <- bfmMagn(bfm05_5000, change=change)
plot(magn.change)
writeRaster(magn.change, filename="magn.change_5000", format="GTiff", overwrite=T)
magn.thresh <- bfmMagn(bfm05_5000, change=change, thresh=-0.05)
plot(magn.thresh)
writeRaster(magn.thresh, filename="magn.thresh_5000", format="GTiff", overwrite=T)
