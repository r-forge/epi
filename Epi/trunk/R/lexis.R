Lexis <-
function(entry, exit, duration, entry.status=0, exit.status=0, id, data,
         merge=TRUE)
{
  nmissing <- missing(entry) + missing(exit) + missing(duration)
  if (nmissing > 1)
    stop("At least two of the arguments entry, exit, duration must be supplied")

  ## If data argument is supplied, use it to evaluate arguments
  if (!missing(data)) {
    if (!missing(entry)) {
      entry <- eval(substitute(entry), data, parent.frame())
    }
    if (!missing(exit)) {
      exit <- eval(substitute(exit), data, parent.frame())
    }
    if (!missing(duration)) {
      duration <- eval(substitute(duration), data, parent.frame())
    }
    entry.status <- eval(substitute(entry.status), data, parent.frame())
    exit.status <- eval(substitute(exit.status), data, parent.frame())
    if (!missing(id)) {
      id <- eval(substitute(id), data, parent.frame())
    }
    if (merge) {
      data <- as.data.frame(data)
    }
  }
  
  ## Coerce entry and exit lists to data frames
  
  if(!missing(entry)) {
    entry <- as.data.frame(entry)
    if (is.null(names(entry)))
      stop("entry times have no names")
    if (any(substr(names(entry),1,4) == "lex."))
      stop("names starting with \"lex.\" cannot be used for time scales")
  }

  if(!missing(exit)) {
    exit <- as.data.frame(exit)
    if (is.null(names(exit)))
      stop("exit times have no names")
    if (any(substr(names(exit),1,4) == "lex."))
      stop("names starting with \"lex.\" cannot be used for time scales")
  }

  if (missing(entry)) {
    ## Impute entry
    entry <- exit - duration
  }
  
  if (missing(duration)) {
    ## Impute duration 
    full.time.scales <- intersect(names(entry), names(exit))
    if (length(full.time.scales) == 0) {
      stop("Cannot calculate duration from entry and exit times")
    }
    duration <- exit[,full.time.scales[1]] - entry[,full.time.scales[1]]
  }

  if (missing(exit)) {
    all.time.scales <- names(entry)
  }
  else {
    ## We dont need the exit times but, if they are supplied, we must
    ## make sure they are consistent with the entry and duration.

    all.time.scales <- unique(c(names(entry), names(exit)))
    ## Fill in any missing entry times
    entry.missing <- setdiff(all.time.scales, names(entry))
    if (length(entry.missing) > 0) {
      entry <- cbind(entry, exit[,entry.missing, drop=FALSE] - duration)
    }
    ## Check that duration is the same on all time scales
    deltat <- exit - entry[,names(exit),drop=FALSE]
    if (missing(duration)) {
      duration <- deltat[,1]
    }
    ok <- sapply(lapply(deltat, all.equal, duration), isTRUE)
    if (!all(ok)) {
      stop("Duration is not the same on all time scales")
    }
  }
  ## Check that duration is positive
  if (any(duration<0)) {
    stop("Duration must be non-negative")
  }
  
  ## Make sure id value - if supplied - is valid. Otherwise supply default id
  
  if (missing(id)) {
    id <- 1:nrow(entry)
  }
  else if (any(duplicated(id))) {
    ##Fixme: check for overlapping intervals
    ##stop("Duplicate values in id")
  }

  ## Return a data frame with the entry times, duration, and status
  ## variables Use the prefix "lex." for the names of reserved
  ## variables.

  lex <- data.frame(entry, "lex.deltat" = duration,
                    "lex.status1"=entry.status,
                    "lex.status2"=exit.status, "lex.id" = id)
  if (!missing(data) && merge) {
    duplicate.names <- intersect(names(lex), names(data))
    if (length(duplicate.names) > 0) {
      stop("Cannot merge data with duplicate names")
    }
    lex <- cbind(lex, data)
  }
  attr(lex,"time.scales") <- all.time.scales
  breaks <- vector("list", length(all.time.scales))
  names(breaks) <- all.time.scales
  attr(lex,"breaks") <- breaks
  class(lex) <- c("Lexis", class(lex))
  return(lex)
}

is.Lexis <- function(x)
{
  inherits(x, "Lexis")
}

check.time.scale <- function(lex, time.scale=NULL)
{

  ##Utility function, returns the names of the time scales in a Lexis object
  ##lex - a Lexis object
  ##time.scale - a numeric or character vector. The function checks that
  ##             these are valid time scales for the Lexis object. 
  ##Return value is a character vector containing the  names of the requested
  ##time scales
  
  all.names <- timeScales(lex)
  if (is.null(time.scale))
    return(all.names)

  nscale <- length(time.scale)
  scale.names <- character(nscale)
  if (is.character(time.scale)) {
    for (i in 1:nscale) {
      if (is.null(lex[[time.scale[i]]]))
        stop("invalid time scale name")
    }
  }
  else if (is.numeric(time.scale)) {
    if (any(time.scale) > length(all.names)  || any(time.scale) < 1)
      stop("invalid time scale column number")
    time.scale <- all.names[time.scale]
  }
  else {
    stop("invalid type for time scale")
  }
  return(time.scale)
}


plot.Lexis.1D <- function(x, time.scale=1, breaks="lightgray", 
                          type="l", col="darkgray", xlim, ylim, xlab, ylab,
                          ...)
{
  ## x Lexis object
  ## time.scale  name of time scale to plot

  if (length(time.scale) != 1)
    stop("Only one time scale allowed")
  
  time.entry <- x[,time.scale]
  time.exit <- x[,time.scale] + x$lex.deltat
  id <- x$lex.id

  if (missing(xlim))
    xlim <- c(min(time.entry), max(time.exit))
  if (missing(ylim))
    ylim <- range(id)
  if (missing(xlab))
    xlab <- time.scale
  if (missing(ylab))
    ylab <- "id number"
  
  plot(time.entry, id, type="n", xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim,
       ...)
  if (type=="b" || type=="l") {
    segments(time.entry, id, time.exit, id, col=col, ...)
  }
  if (type=="b" || type=="p") {
    points(time.exit, id, col=col, ...)
  }
  ## Plot break points
  brk <- attr(x,"breaks")[[time.scale]]
  abline(v=brk, col=breaks, ...)
}

points.Lexis.1D <- function(x, time.scale, ...)
{
  time.exit <- x[,time.scale] + x$lex.deltat
  points(time.exit, x$lex.id, ...)
}

lines.Lexis.1D <- function(x, time.scale, col="darkgray", breaks="lightgray",
                           ...)
{
  time.entry <- x[,time.scale]
  time.exit <- x[,time.scale] + x$lex.deltat
  id <- x$lex.id
  segments(time.entry, id, time.exit, id, col=col, ...)
  ## Plot break points
  brk <- attr(x,"breaks")[time.scale]
  abline(v=brk[[1]], h=brk[[2]], col=breaks, ...)
}

plot.Lexis.2D <- function(x, time.scale, breaks="lightgray",
                          type="l", col="darkgray",
                          xlim, ylim, xlab, ylab,
                          grid=FALSE,
                      col.grid="lightgray",
                      lty.grid=2,
                      coh.grid=FALSE,
                          ...)
{
  if (length(time.scale) != 2)
    stop("Two time scales are required")

  time.entry <- time.exit <- vector("list",2)
  for (i in 1:2) {
    time.entry[[i]] <- x[,time.scale[i]]
    time.exit[[i]] <- x[,time.scale[i]] + x$lex.deltat
  }

  if (missing(xlim) && missing(ylim)) {
    ## If no axis limits are given, set the plotting region to be
    ## square, and adjust the axis limits to cover the same time interval.
    ## All life lines will then be at 45 degrees.
    opar <- par(pty="s")
    on.exit(par(opar))
    min.times <- sapply(time.entry, min)
    max.times <- sapply(time.exit, max)
    xywidth <- max(max.times - min.times)
    xlim <- min.times[1] + c(0, xywidth)
    ylim <- min.times[2] + c(0, xywidth)
  }
  else if (missing(xlim)) {
    xlim <- c(min(time.entry[[1]]), max(time.exit[[1]]))
  }
  else if (missing(ylim)) {
    ylim <- c(min(time.entry[[2]]), max(time.exit[[2]]))
  }
  
  if (missing(xlab))
    xlab <- time.scale[1]
  if (missing(ylab))
    ylab <- time.scale[2]
      
  plot(time.entry[[1]], time.entry[[2]], type="n",
       xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, ...)

# Set up the background grid(s):
  if (!missing(grid)) {
     if (is.logical(grid)) {
        if (grid) {
        vgrid <- pretty(xlim)
        hgrid <- pretty(ylim)
        } }
     else if (is.list(grid)) {
        vgrid <- grid[[1]]
        hgrid <- grid[[length(grid)]]
        }
     else if (is.numeric(grid)) {
       vgrid <- grid - min( grid ) + min( pretty( xlim )[pretty(xlim)>=par("usr")[1]] )
       hgrid <- grid - min( grid ) + min( pretty( ylim )[pretty(ylim)>=par("usr")[3]] )
        }
     else stop( "'grid' must be either logical, list or a numeric vector" )
   # and plot the grid:
     abline( v=vgrid, h=hgrid, col=col.grid, lty=lty.grid )
     box()
     }
  if (!missing(grid) & coh.grid) {
     # Make the 45-degree grids as fine as the finest grid on the axes
     for (yy in c(hgrid-diff(range(hgrid)),hgrid))
         abline( yy-min(vgrid), 1, col=col.grid, lty=lty.grid )
     for (yy in c(vgrid-diff(range(vgrid)),vgrid))
         abline( min(hgrid)-yy, 1, col=col.grid, lty=lty.grid )
     }
# End of background grid(s) (PHEW!)

  if (type=="b" || type=="l") {
    segments(time.entry[[1]], time.entry[[2]], time.exit[[1]], time.exit[[2]],
             col=col, ...)
  }
  if (type=="b" || type=="p") {
    points(time.exit[[1]], time.exit[[2]], col = col, ...)
  }
  if (type != "n") {
    ## Plot break points
    brk <- attr(x,"breaks")[time.scale]
    abline(v=brk[[1]], h=brk[[2]], col=breaks, ...)
  }
}

points.Lexis.2D <- function(x, time.scale, ...)
{
  time.exit <- vector("list",2)
  for (i in 1:2) {
    time.exit[[i]] <- x[,time.scale[i]] + x$lex.deltat
  }
  points(time.exit[[1]], time.exit[[2]], ...)
}

lines.Lexis.2D <- function(x, time.scale, col="darkgray", ...)
{
  time.entry <- time.exit <- vector("list",2)
  for (i in 1:2) {
    time.entry[[i]] <- x[,time.scale[i]]
    time.exit[[i]] <- x[,time.scale[i]] + x$lex.deltat
  }
  segments(time.entry[[1]], time.entry[[2]], time.exit[[1]], time.exit[[2]],
           col=col, ...)
}

### Plotting generic functions

plot.Lexis <- function(x, time.scale=NULL, breaks="lightgray", ...)
{
  time.scale <- check.time.scale(x, time.scale)
  if (length(time.scale) > 2)
    time.scale <- time.scale[1:2]
  
  if (length(time.scale) == 1)
    plot.Lexis.1D(x, time.scale=time.scale, breaks=breaks, ...)
  else if (length(time.scale) == 2)
    plot.Lexis.2D(x, time.scale=time.scale, breaks=breaks, ...)
}


lines.Lexis <- function(x, time.scale=NULL, ...)
{
  time.scale <- check.time.scale(x, time.scale)
  if (length(time.scale) > 2)
    time.scale <- time.scale[1:2]
  
  if (length(time.scale) == 1)
    lines.Lexis.1D(x, time.scale=time.scale, ...)
  else if (length(time.scale) == 2)
    lines.Lexis.2D(x, time.scale=time.scale, ...)
}

points.Lexis <- function(x, time.scale = NULL, ...)
{
  time.scale <- check.time.scale(x, time.scale)
  if (length(time.scale) > 2)
    time.scale <- time.scale[1:2]
  
  if (length(time.scale) == 1)
    points.Lexis.1D(x, time.scale=time.scale, ...)
  else if (length(time.scale) == 2)
    points.Lexis.2D(x, time.scale=time.scale, ...)
}

### Generic functions

### Methods for data.frame drop Lexis attributes, so we need a Lexis
### method that adds them again

subset.Lexis <- function(x, ...)
{
  y <-  subset.data.frame(x, ...)
  attr(y,"breaks") <- attr(x, "breaks")
  attr(y,"time.scales") <- attr(x, "time.scales")
  return(y)
}

merge.data.frame <- function(x, y, ...)
{
  if (is.Lexis(x))
    merge.Lexis(x, y, ...)
  else if (is.Lexis(y)) 
    merge.Lexis(y, x, ...)
  else
    base::merge.data.frame(x, y, ...)
}

merge.Lexis <- function(x, y, id, by, ...)
{
  if (!missing(id)) {
    if (!is.character(id) || length(id) != 1 || !(id %in% names(y))) {
      stop("id must be the name of a single variable in y")
    }
    if (any(duplicated(y[[id]]))) {
      stop("values of the id variable must be unique in y")
    }
    y$lex.id <- y[[id]]
  }
  else if (missing(by)) {
    by <- intersect(names(x), names(y))
    if (length(by)==0) {
      stop("x and y have no variable names in common")
    }
  }
  
  z <-  base::merge.data.frame(x, y)
  attr(z,"breaks") <- attr(x, "breaks")
  attr(z,"time.scales") <- attr(x, "time.scales")
  class(z) <- c("Lexis", "data.frame")
  return(z)
}


entry <- function(x, time.scale = NULL)
{
  time.scale <- check.time.scale(x, time.scale)
  return(x$time.scale[1])
}

exit <- function(x, time.scale = NULL)
{
  time.scale <- check.time.scale(x, time.scale)
  return(x$time.scale[1] + x$lex.deltat)
}

deltat.Lexis <- function(x, ...)
{
  return(x$lex.deltat)
}

status <- function(x, at=c("exit","entry"))
{
  switch(match.arg(at),
         "entry"=x$lex.status1,
         "exit"=x$lex.status2)
}
  
  
## Other extractor functions

timeScales <- function(x)
{
  return (attr(x,"time.scales"))
}


timeBand <- function(lex, time.scale,  type)
{
  time.scale <- check.time.scale(lex, time.scale)[1]
  breaks <- attr(lex, "breaks")[[time.scale]]

  time1 <- lex[[time.scale]]
  band <- findInterval(time1, breaks)

  ##Check that right hand side of interval falls in the same band
  abrk <- c(breaks, Inf)
  if (any(time1 + lex$lex.deltat > abrk[band+1])) {
    stop("Intervals spanning multiple time bands in Lexis object")
  }

  type <- match.arg(type, choices = c("factor","integer","left","middle",
                    "right"))
  if (type=="integer") {
     return(band)
  }

  I1 <- c(-Inf, breaks)
  I2 <- c(breaks, Inf)
  labels <- switch(type,
                   "factor" = paste("(", I1, ",", I2, "]", sep=""),
                   "left" = I1,
                   "right" = I2,
                   "middle" = (I1 + I2)/2)
                   
  if(type=="factor") {
    return(factor(band, levels=0:(length(breaks)+1), labels=labels))
  }
  else {
    return(labels[band+1])
  }
}

breaks <- function(lex, time.scale)
{
  time.scale <- check.time.scale(lex, time.scale)[1]
  return(attr(lex, "breaks")[[time.scale]])
}
