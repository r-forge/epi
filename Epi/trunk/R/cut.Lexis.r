cut.Lexis <-
function( data,
           cut,
     timescale = timeScales(data)[1],
     new.state = max( c(data$lex.status1,data$lex.status2) )+1,
        na.cut = Inf )
{
if( !inherits( data, "Lexis" ) )
    stop("First argument must be a Lexis dataect, but it is a ", class(data) )
if( !( timescale %in% (tscal<-timeScales(data)) ) )
    stop("You must spcify one of the valid timescales:",
                tscal )
if( length( cut )==1 ) cut <- rep(cut,nrow(data))
if( length( cut ) != nrow( data ) )
    stop( "'cut' must have length 1 or nrow(data) (=", nrow(data), "), not ", length(cut) )
if( length( new.state )==1 ) new.state <- rep(new.state,nrow(data))
if( length( new.state ) != nrow( data ) )
    stop( "'new.state' must have length 1 or nrow(data) (=", nrow(data), "), not ", length(new.state) )

# Missing cutpoints. Default is to treat as +Inf
cut[is.na(cut)] <- na.cut

# First intervals
in.1 <- data[,timescale]
ex.1 <- pmin( cut, data[,timescale]+data$lex.deltat, na.rm=T )
lx.1 <- data
lx.1$lex.deltat  <- ex.1 - in.1
lx.1$lex.status2 <- ifelse( lx.1$lex.deltat==data$lex.deltat,
                            data$lex.status2,
                            new.state )

# Last intervals
in.2 <- pmax( cut, data[,timescale], na.rm=TRUE )
ex.2 <- data[,timescale]+data$lex.deltat
lx.2 <- data
# Length of 2nd intervals
lx.2$lex.deltat  <- ex.2 - in.2
# Update timescale starting points
lx.2[,timeScales(data)] <- lx.2[,timeScales(data)] + lx.1$lex.deltat
# New entry status
lx.2$lex.status1 <- new.state
# Exit status is the new state if the original record had no transition
lx.2$lex.status2 <- ifelse( data$lex.status2 == data$lex.status1,
                            new.state,
                            data$lex.status2 )

# Combine and restrict to the records with positive follow-up
lx <- rbind( lx.1, lx.2 )
lx <- lx[lx$lex.deltat>0,]
return( lx[order(lx$lex.id,lx[,timescale]),] )
}
