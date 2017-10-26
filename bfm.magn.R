bfm.magn <- function(bfm, change=NULL, thresh=NULL, filename="", format="", overwrite=FALSE)
# extracts the change magnitude raster layer from the bfm results brick
# the second layer of the brick contains the change magnitude values
# optional: supply a change raster layer (see bfm.change()) from which to filter results
    # ie. look only at change magnitudes of breakpoint pixels, rather than a continuous field
# optional: define a threshold (thresh) above which values are filtered out (e.g. for looking only at significant negative changes)
{
  # extract magn raster layer
  magn <- raster(bfm, 2)
  
  # include only identified breakpoint pixels
  if(!is.null(change))
    magn[is.na(change)] <- NA
  
  # apply a threshold (only take values below this threshold)
  if (!is.null(thresh))
    magn[magn>thresh] <- NA
  
  # write to file
  if(filename!="")
    writeRaster(magn, filename=filename, format=format, overwrite=overwrite)
  
  return(magn)
}