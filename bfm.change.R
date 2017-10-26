bfm.change <- function(bfm, filename="", format="", overwrite=FALSE)
# extracts a breakpoint timing value for each pixel of the bfm output brick 
# bfm is the raster brick output from the spatial bfastmonitor() wrapper function
# the first layer of the brick contains the breakpoint timing data
{
  
  change <- raster(bfm, 1)
  
  if(filename!="") 
    writeRaster(change, filename=filename, format=format, overwrite=overwrite)
  
  return(change)
}