within.extent <- function(coords, ext, tolerance=NULL){
  
  if(is.null(tolerance)) tolerance <- 0
  
  coords <- as.vector(coords)
  if (length(coords) != 2 & length(coords) != 4)
    stop("coords must be of length 2 (coord pair) or 4 (extent vector)")
  if (length(coords)==2)
    test <- 
    coords[1] >= xmin(ext) - tolerance & 
    coords[1] <= xmax(ext) + tolerance & 
    coords[2] >= ymin(ext) - tolerance & 
    coords[2] <= ymax(ext) + tolerance
  
  if (length(coords)==4)
    test <- 
    coords[1] >= xmin(ext) - tolerance &
    coords[1] <= xmax(ext) &
    coords[2] >= xmin(ext) &
    coords[2] <= xmax(ext) + tolerance &
    coords[3] >= ymin(ext) - tolerance &
    coords[3] <= ymax(ext) &
    coords[4] >= ymin(ext) &
    coords[4] <= ymax(ext) + tolerance
  
  return(test)
}