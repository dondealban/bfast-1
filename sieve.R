sieve <- function(x, rm.diagonal=FALSE, filename="", ...)
# filter out 'lonely' pixels from a raster layer
{
  # create unit raster with same attributes as input raster
  y <- raster(x)
  y[!is.na(x)] <- 1
  
  # 1) default: all adjacent cells are considered (including diagonals)
  if(!rm.diagonal){
      # w=3 weighs all neighbours equally
      z <- focal(y, 3, sum, na.rm=TRUE)
      # remove all cells with no neighbours (focal(x)==1)
  }
  # 2) diagonal cells do not qualify as 'adjacency'
  else{
    # with this matrix, diagonals do not count in the focal sum
    w <- rbind(c(0,1,0),
               c(1,1,1),
               c(0,1,0))
    z <- focal(y, w, sum, na.rm=TRUE)
  }
  # apply the filter on the target raster
  x[z==1] <- NA
  
  # optional: write output to file
  if(!filename=="")
    writeRaster(x, filename, ...)
  
  return(x) 
}