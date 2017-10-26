genGridUTM <-
function(tilesize, extent, res, overlap=NULL, polygon=FALSE, prj=NULL)
  # generates a matrix of tile extents: ie. (xmin, xmax, ymin, ymax) for each tile
    # args:
        # tilesize - size of the output tiles (in # pixels)
        # extent - full extent of the scene (as a raster extent object)
        # res - spatial resolution of the image
            # this can also be supplied using res(x), which returns a vector of length 2, in which case the first element is taken to be the resolution
        # overlap - overlapping region in # pixels (optional)
{
    # take the first element of res, if length > 1
    if(length(res) > 1)
        res <- res[1]
    
    # set overlap value (in m)
    if(is.null(overlap))
      overlap <- 0
    
    # divide overlap by 2, since extension will go in both directions
    overlap <- overlap / 2
    
    # generate xmin and ymin values for each tile
    xmins <- seq(xmin(extent), xmax(extent), by=tilesize*res)
    ymins <- seq(ymin(extent), ymax(extent), by=tilesize*res)
    
    # generate xmax and ymax values for each tile
    xmaxes <- c(xmins[2:length(xmins)], xmax(extent))
    ymaxes <- c(ymins[2:length(ymins)], ymax(extent))
    
    # extend extents by overlap
    if (!is.null(overlap)){
      xmins <- c(xmins[1], xmins[2:length(xmins)] - overlap*res)
      ymins <- c(ymins[1], ymins[2:length(ymins)] - overlap*res)
      xmaxes <- c(xmaxes[1:length(xmaxes)-1] + overlap*res, xmaxes[length(xmaxes)])
      ymaxes <- c(ymaxes[1:length(ymaxes)-1] + overlap*res, ymaxes[length(ymaxes)])
    }

    # permute mins and maxes to get all possible extents          
    tiles <- expand.grid(xmins, ymins)
    colnames(tiles) <- c("xmin", "ymin")
    
    temp <- expand.grid(xmaxes, ymaxes)
    tiles$xmax <- temp[,1]
    tiles$ymax <- temp[,2]
        
    # reorder the columns so that each row matches a raster extent object
    tiles <- cbind(tiles$xmin, tiles$xmax, tiles$ymin, tiles$ymax)
    colnames(tiles) <- c("xmin", "xmax", "ymin", "ymax")
    
    # generate tile names that can be used when writing subsets to file
        # in the form "row.column", starting from the bottom left tile ("1.1")
    ind <- NULL
    for (j in 1:length(ymins)){
        for (i in 1:length(xmins)){
          ind <- c(ind, paste(j,".",i,sep=""))
        }
    }
    row.names(tiles) <- ind
    
    # delete redundant rows (ie. where xmin==xmax or ymin==ymax)
    #tiles <- tiles[-which(tiles[,1]==tiles[,2] | tiles[,3]==tiles[,4]),]
        
    # optional: output a SpatialPolygons object for visualization of tiles
    if(polygon){
      
      if(is.null(prj)){
        warning("No projection provided. Defaulting to WGR1984 UTM zone 1")
        prj <- CRS("+proj=utm +zone=1 +ellps=WGS84 +units=m +no_defs")
      }
      
      poly <- as.extentPolygon(tiles[1,], prj, ID=ind[1])
      
      for(i in 2:nrow(tiles)){
        poly <- spRbind(poly, as.extentPolygon(tiles[i,], prj, ID=ind[i]))
      }
    }
    
    if(polygon)
      tiles <- list(tiles=tiles, poly=poly)
      
    return(tiles)
}
