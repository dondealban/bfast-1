run_bfm <-
  function(data, history="all", monperiod=c(), monend="full", sensor="all", formula=response~trend+harmon, order=1, 
           sceneID=NULL, filename="", overwrite=FALSE)
  {
    
    # errors
    if(is.null(monperiod)) 
      stop("missing value for monperiod (ie. start of the monitoring period)")
    if(monend[1]!="full" & !is.numeric(monend)) 
      stop("monend should either be \'full\' or numeric of length=2 (last date of monitoring period)")
 
    b <- data
    
    # select layers based on sensor
    # NOTE: this only works if raster names correspond with Landsat sceneID's; otherwise use sensor="all"
    if (sensor!="all"){
      s <- get.sceneinfo(names(b))
      
      # if a character vector is supplied
      if("ETM+" %in% sensor)
        sensor <- c(sensor, "ETM+ SLC-on", "ETM+ SLC-off")
      layerSelect <- which(s$sensor %in% sensor)
      
      # if a numeric vector is supplied
      if(is.numeric(sensor))
        layerSelect <- which(substr(row.names(s), 3, 3) %in% sensor)
      
      # trim the brick to use only these layers; reassign layer names
      layerDrop <- c(1:nlayers(b))[-layerSelect]
      b <- dropLayer(b, layerDrop)
      names(b) <- row.names(s)[layerSelect]
    }
        
    # get dates and years from sceneID vector or calculate dates and years from layer names
    if(is.null(sceneID))
      sceneID <- row.names(get.sceneinfo(names(b)))
    dates <- get.sceneinfo(sceneID)$date
    
    # trim time series if monend!="full"
    if(monend[1]!="full"){
      end.date <- as.Date(paste(monend[1], monend[2], sep="-"), format="%Y-%j")
      b <- dropLayer(b, which(dates > end.date))
      
      # redefine dates based on trimmed raster brick
      sceneID <- sceneID[which(dates <= end.date)]
      dates <-get.sceneinfo(sceneID)$date
    }
    
    # declare bfm helper functions
    # TODO: rewrite ybfastmonitor to include bfm.pixel()?
    ybfastmonitor_new <- function(x, dates) {
      vi <- bfastts(x, dates, type = c("irregular"))
      vi[vi <= 0] <- NA
      bfm <- bfastmonitor_new(formula = formula, order = order, 
                              data = vi, start = monperiod, history = history)

      return(cbind(bfm$breakpoint, bfm$magnitude, (bfm$history[2] - 
                                                     bfm$history[1])))
    }
    fun <- function(y) {
      percNA <- apply(y, 1, FUN = function(x) (sum(is.na(x))/length(x)))
      i <- ((percNA < 1)) # logical vector corresponding to pixel ts
      res <- matrix(NA, length(i), 3)
      
      # do not apply bfm if there are only NA's in the pixel ts
      if (sum(i) > 0) {
        res[i, ] <- t(apply(y[i, ], 1, ybfastmonitor_new, dates))
      }
      res
    }
    
    if(filename!="")
      x <- calc(b, fun=fun, filename=filename, overwrite=overwrite)
    else
      x <- calc(b, fun=fun)
    
    return(x)
  }