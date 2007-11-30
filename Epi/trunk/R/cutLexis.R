cutLexis <- function(data,
                     cut,
                     timescale = timeScales(data)[1],
                     new.state,
                     count=FALSE,
                     na.cut = Inf)
{
    if (!inherits(data, "Lexis"))
      stop("First argument must be a Lexis object, but it is a ", class(data) )
    if (!( timescale %in% (tscal<-timeScales(data))))
      stop("You must specify one of the valid timescales: ", tscal )
    if (length(cut)==1) cut <- rep(cut,nrow(data))
    if (length(cut) != nrow(data))
      stop( "'cut' must have length 1 or nrow(data) (=", nrow(data),
           "),\n --- but it has length ", length(cut),"." )

    ## Should states be returned as a factor at exit?
    states.as.factor <- is.factor(data$lex.Cst) || is.factor(data$lex.Xst)

    if (missing(new.state)) {
        if (count) {
            if (!is.numeric(data$lex.Cst)) {
                stop("count can only be used with a numeric state")
            }
            new.state <- data$lex.Cst + 1
        }
        else {
            new.state <- data$lex.Cst
        }
    }
    else {
        if (length(new.state) == 1) {
            new.state <- rep(new.state,nrow(data))
        }
        if (length(new.state) != nrow(data)) {
            stop("'new.state' must have length 1 or nrow(data) (=", nrow(data),
                 "),\n --- but it has length ", length(new.state))
        }
        states.as.factor <- states.as.factor || is.character(new.state)
        if (count) {
            warning("count argument ignored")
        }
        count = FALSE
    }
    
    ## Transform all to factors with the ame set of levels,
    ## and then to numeric for the calculations
    if( !is.character( new.state ) ) new.state <- as.character( new.state )
    new.states <- unique( new.state )
    data$lex.Cst <- factor( data$lex.Cst )
    data$lex.Cst <- factor( data$lex.Cst )
    all.states <- c(unique(c(levels(data$lex.Cst),
                             levels(data$lex.Xst))),new.states)
    print(all.states)
    new.state <- match( new.state, all.states )
    print(table(new.state))
    data$lex.Cst <- match( data$lex.Cst, all.states )
    data$lex.Xst <- match( data$lex.Xst, all.states )

    ## Missing cutpoints. Default is to treat as +Inf
    cut[is.na(cut)] <- na.cut

    ## First intervals
    in.1 <- entry( data, timescale )
    ex.1 <- pmin( cut, exit( data, timescale ), na.rm=T )
    lx.1 <- data
    lx.1$lex.dur  <- as.vector(ex.1 - in.1)
    lx.1$lex.Xst <- ifelse( ex.1 == as.vector( exit( data, timescale ) ),
                           data$lex.Xst,
                           new.state )

    ## Last intervals
    in.2 <- pmax( cut, entry( data, timescale ), na.rm=TRUE )
    ex.2 <- exit( data, timescale )
    lx.2 <- data
    ## Length of 2nd intervals i.e. AFTER the cut
    lx.2$lex.dur  <- as.vector(ex.2 - in.2)
    ## Update timescale starting points by subtracting from the endpoint
    lx.2[,timeScales(lx.2)] <- exit(data) - lx.2$lex.dur

    ## Note: we don't have to bother about intervals entirely to the
    ## left of cut. They are removed below
    lx.2$lex.Cst <- new.state
    ## Exit status is unchanged, unless we are using counting notation
    lx.2$lex.Xst <- if (count) {
        data$lex.Xst + 1
    }
    else {
        data$lex.Xst
    }
    
    ## Combine and restrict to the records with positive follow-up
    lx <- rbind( lx.1, lx.2 )
    lx <- lx[lx$lex.dur>0,]
    if(states.as.factor) {
        lx$lex.Cst <- factor(lx$lex.Cst, levels=1:length(all.states),
                             labels=all.states )
        lx$lex.Xst <- factor(lx$lex.Xst, levels=1:length(all.states),
                             labels=all.states )
    }
    return( lx[order(lx$lex.id,lx[,timescale]),] )
}
