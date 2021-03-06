get.sceneinfo <- function(sourcefile, filename="")
  # returns a data.frame with sensor, path, row, and date information for each scene
  # writes to a .csv if a filename/path is specified
{
  # for the sake of convenience, sourcefile can be either a character vector of scene names (or subfolders) or the original .tar.gz files
  # this will check which it is and format accordingly
  sourcefile <- sapply(sourcefile, FUN=function(x){if(grepl(".gz", x)) x <- substr(x, 1, nchar(x)-3);
                                                   if(grepl(".tar", x)) x <- substr(x, 1, nchar(x)-4); 
                                                   return(x)})  
  
  # extract year and julian day
  year <- substr(sourcefile, 10, 13)
  day <- substr(sourcefile, 14, 16)
  # format date as "%Y-%m-%d" (ie. yyyy-mm-dd)
  dates <- as.Date(paste(year, day, sep=""), format="%Y%j")
  
  # identify the sensor
  sensor <- as.character(mapply(substr(sourcefile, 1, 3), dates, FUN=function(x,y){
      if(x=="LE7" & y<="2003-03-31")
          "ETM+ SLC-on"
      else if(x=="LE7" & y>"2003-03-31")
          "ETM+ SLC-off"
      else if(x=="LT5" | x=="LT4") 
          "TM" 
      else if(x=="LC8")
          "OLI"
      else
          stop(sourcefile, " is not a recognized Landsat5/7/8 scene ID.")
      }))
  # extract path, row
  path <- as.numeric(substr(sourcefile, 4, 6))
  row <- as.numeric(substr(sourcefile, 7, 9))

  # throw all data into a data.frame
  info <- data.frame(sensor=sensor, path=path, row=row, date=dates)
  row.names(info) <- sourcefile
  
  # optional: print to .csv for future reference
  if(filename!="") write.csv(info, file=filename, quote=FALSE)
  
  return(info)
  
}