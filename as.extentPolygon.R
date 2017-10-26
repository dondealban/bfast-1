as.extentPolygon <- function(ext, prj, ID=NULL, plot=FALSE)
# converts a raster extent object to a SpatialPolygons object (sp)
{
  ext <- extent(ext) # in case a vector is supplied instead
  
  coords <- matrix(ncol=2, nrow=5)
  coords[1,] <- c(xmin(ext), ymin(ext)) # bottom-left
  coords[2,] <- c(xmin(ext), ymax(ext)) # top-left
  coords[3,] <- c(xmax(ext), ymax(ext)) # top-right
  coords[4,] <- c(xmax(ext), ymin(ext)) # bottom-right
  coords[5,] <- coords[1,] # close the polygon
  
  # make a SpatialPolygons object from the coords matrix
  if(is.character(prj))
    prj <- CRS(prj)
  if(is.null(ID))
    ID <- "ID"
  poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=ID)), 
                          proj4string=prj)
  
  if(plot)
    plot(poly)
  
  return(poly)
}