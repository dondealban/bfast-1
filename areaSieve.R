areaSieve <- function(x, thresh=5000, filename=NULL, verbose=FALSE, ...)
  # TO DO: make this more generic by supplying # of pixels as parameter, then calculating the actual area based on prj
{
  poly <- gdal_polygonizeR(x)
  area <- numeric(nrow(poly))
  for(j in 1:nrow(poly)){area[j] <- poly[j,]@polygons[[1]]@Polygons[[1]]@area}
  poly <- poly[-which(area<thresh),]
  if (verbose) 
    cat("Applying the sieve...\n")
  mask <- rasterize(poly, x, ...)
  y <- x
  y[is.na(mask)] <- NA
  return(y)
}