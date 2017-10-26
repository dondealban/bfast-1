bfm.pixel <- function (x, monperiod = c(), monend="full", cell = c(), history = "all", min.thresh = NULL, sensor="all",
                       formula = response ~ trend + harmon, order = 1, interactive = FALSE, 
                       plot = FALSE) 
{
    # layer names of the input raster brick must correspond to LS scene names!
    dates <- get.sceneinfo(names(x))$date
    
    # limit analysis to specific sensor(s)
    if (sensor != "all") {
        if ("ETM+" %in% sensor) 
            sensor <- c(sensor, "ETM+ SLC-on", "ETM+ SLC-off")
        layerSelect <- which(s$sensor %in% sensor)
        if (is.numeric(sensor)) 
            layerSelect <- which(substr(row.names(s), 3, 3) %in% 
                sensor)
        layerDrop <- c(1:nlayers(x))[-layerSelect]
        x <- dropLayer(x, layerDrop)
        s <- s[layerSelect, ]
        names(x) <- row.names(s)
    }
    
    # trim time series if monend is supplied
    if (monend[1] != "full") {
        end.date <- as.Date(paste(monend[1], monend[2], sep = "-"), 
            format = "%Y-%j")
        x <- dropLayer(x, which(s$date > end.date))
        s <- s[-which(s$date > end.date), ]
    }
    
    # select cell from the input raster brick x in 1 of 3 ways:
    # 1) interactively (by clicking on an already plotted map)
    # 2) by supplying the cell index as an integer of length=1
    # 3) by supplying a vector of length=2 representing the (x,y) coordinates
    if (interactive) 
        cell <- as.data.frame(click(x, n = 1, id = TRUE, cell = TRUE))$cell
    else cell <- ifelse(length(cell) == 2, cellFromXY(x, t(as.matrix(cell))), 
                        cell)
    
    # extract pixel time series
    ndvi <- bfastts(as.vector(x[cell]), dates, type = c("irregular"))
    # apply a treshold (if supplied)
    if (!is.null(min.thresh)) 
        ndvi[ndvi <= min.thresh] <- NA
    
    # run bfm on the pixel time series
    bfm <- bfastmonitor(formula = formula, order = order, data = ndvi, 
                        start = monperiod, history = history)
    
    ## make a plot in ggplot2
    p <- ggplot(data=bfm$tspp, aes(x=time, y=response)) +
        geom_line() +
        geom_point(size=2) +
        geom_line(aes(y=prediction), colour="blue") +
        ylab("NDVI") + 
        geom_vline(xintercept=bfm$history[2], linetype="dashed")
    # label breakpoint if there is one
    if(!is.na(bfm$breakpoint))
        p <- p + geom_vline(xintercept=bfm$breakpoint, colour="red", linetype="dotted")
    
    # return list of bfm results, cell number (for follow-up tests on the same pixel) and ggplot object (if TRUE)
    if (plot){
        return(list(bfm = bfm, cell = cell, plot = p))
        print(p)
    }
    else
        return(list(bfm = bfm, cell = cell))
}