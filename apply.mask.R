apply.mask <- function(target, mask, maskvalue=c(), filename="", format="", overwrite=FALSE)
{
  require(raster)
  require(rgdal)

  if(is.null(maskvalue)) stop("Supply one or more values for maskvalue.")
  
  subNA <- function(x,y){x[y%in%maskvalue] <- NA; return(x)}
  
  target <- crop(target, mask)
  
  if(filename=="")
    output <- overlay(target, mask, fun=function(x,y) subNA(x,y))
  else
    output <- overlay(target, mask, fun=function(x,y) subNA(x,y), filename=filename, format=format, overwrite=overwrite)
  
  return(output)
}