subsetUTM <-
function(data, tilesize, overlap=NULL, filename="", fileext="", format="", overwrite=FALSE)
  # subset a scene or AOI based on a predefined tile size
  # args:
    # data - a raster object to be subsetted
    # tilesize - desired tile size (in # of pixels)
    # overlap - overlapping region in # pixels (optional)
    # filename - the (prefix of) the filename to write to (NOTE: NOT including file extension!)
        # a suffix with the tile index ("row.column") will be added to the filename
    # fileext - the extension of the files to be written (not including the ".")
    # format - the file format to save to (see ?writeRaster for more info)
    # overwrite - overwrite existing files?
  # TO DO:
      # - improve file naming system...
      # - allow filename to be supplied as a vector of filenames, or create an option for automatic filenaming (based on tile ID)
{
  # generate a data.frame of subset extents
  tiles <- genGridUTM(tilesize, extent(data), res(data), overlap)
  
  # crop the input raster according to above extents and store in a list of raster objects
  img <- vector("list", nrow(tiles))
  for (i in 1:nrow(tiles)){
    img[[i]] <- crop(data, extent(tiles[i,]))
    names(img[[i]]) <- paste(names(data), "_", row.names(tiles)[i], sep="")
    
    if (filename!="")
      writeRaster(img[[i]], filename=paste(filename, "_", row.names(tiles)[i], ".", fileext, sep=""),
                  format=format, overwrite=overwrite)
  }
  
  # return list of subsetted rasters
  return(img)
}
