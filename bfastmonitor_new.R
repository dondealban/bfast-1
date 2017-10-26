bfastmonitor_new <- function (data, start, formula = response ~ trend + harmon, order = 1, 
          lag = NULL, slag = NULL, history = c("ROC", "BP", "all"), 
          type = "OLS-MOSUM", h = 0.25, end = 10, level = 0.05, hpc = "none", 
          verbose = FALSE, plot = FALSE) 
# this code has been altered slightly to assign -9999 when data availability requirements are not met (rather than returning an error)
# it's a bandaid solution for now, so suggestions on how to adapt this for spatiotemporal analysis are more than welcome!
{
    level <- rep(level, length.out = 2)
    if (!is.ts(data)) 
        data <- as.ts(data)
    freq <- frequency(data)
    time2num <- function(x) if (length(x) > 1L) 
        x[1L] + (x[2L] - 1)/freq
    else x
    start <- time2num(start)
    data_tspp <- bfastpp(data, order = order, lag = lag, slag = slag)
    history_tspp <- subset(data_tspp, time < start)
    if (is.null(history)) {
        history <- start(history_tspp$response)
    }
    else if (all(is.character(history))) {
        history <- match.arg(history)
        history <- switch(history, all = start(history_tspp$response), 
                          ROC = history_roc(formula, data = history_tspp, level = level[2]), 
                          BP = history_break(formula, data = history_tspp, 
                                             hpc = hpc))
    }
    else if (all(is.function(history))) {
        history <- history(formula, data = history_tspp)
    }
    history <- time2num(history)
    history_tspp <- subset(history_tspp, time >= history)
    if (verbose) {
        cat("\nBFAST monitoring\n\n1. History period\n")
        cat(sprintf("Stable period selected: %i(%i)--%i(%i)\n", 
                    start(history_tspp$response)[1], start(history_tspp$response)[2], 
                    end(history_tspp$response)[1], end(history_tspp$response)[2]))
        cat(sprintf("Length (in years): %f\n", NROW(history_tspp)/freq))
    }
    test_tspp <- history_tspp
    test_mefp <- mefp(formula, data = test_tspp, type = type, 
                      period = end, h = h, alpha = level[1])
    test_lm <- lm(formula, data = test_tspp)
    
    # this has been altered to assign magn=-9999 instead of stopping bfm
    if (floor(h * NROW(test_tspp)) <= 1 | NROW(test_tspp) <= 
            length(coef(test_lm))) {
        rval <- list(data = NA, tspp = NA, model = NA, mefp = NA, 
                     history = c(NA, NA), monitor = c(NA, NA), breakpoint = NA, 
                     magnitude = -9999)
    }
    else {
        if (verbose) {
            cat("Model fit:\n")
            print(coef(test_lm))
        }
        test_tspp <- subset(data_tspp, time >= history)
        test_mon <- monitor(test_mefp, data = test_tspp, verbose = FALSE)
        tbp <- if (is.na(test_mon$breakpoint)) 
            NA
        else test_tspp$time[test_mon$breakpoint]
        if (verbose) {
            cat("\n\n2. Monitoring period\n")
            cat(sprintf("Monitoring starts at: %i(%i)\n", floor(start), 
                        round((start - floor(start)) * freq) + 1))
            if (is.na(tbp)) {
                cat("Break detected at: -- (no break)\n\n")
            }
            else {
                cat(sprintf("Break detected at: %i(%i)\n\n", 
                            floor(tbp), round((tbp - floor(tbp)) * freq) + 
                                1))
            }
        }
        test_tspp$prediction <- predict(test_lm, newdata = test_tspp)
        new_data <- subset(test_tspp, time >= start)
        magnitude <- median(new_data$response - new_data$prediction, 
                            na.rm = TRUE)
        rval <- list(data = data, tspp = test_tspp, model = test_lm, 
                     mefp = test_mon, history = c(head(history_tspp$time, 
                                                       1), tail(history_tspp$time, 1)), monitor = c(start, 
                                                                                                    tail(test_tspp$time, 1)), breakpoint = tbp, magnitude = magnitude)
        class(rval) <- "bfastmonitor"
        if (plot) 
            plot(rval)
    }
    return(rval)
}