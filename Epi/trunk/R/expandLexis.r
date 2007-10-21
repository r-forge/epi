expandLexis <-
function( obj, timescale=timeScales(obj)[1] )
{
tr.mat <- tabLexis( obj )
# An empty Lexis object with the
new <- NULL
for( i in 1:nrow(tr.mat) )
for( j in 1:ncol(tr.mat) )
{
if( tr.mat[i,j] > 0 )
  {
  tmp <- obj[obj$lex.status1==rownames(tr.mat)[i],]
  tmp$status <- ( tmp$lex.status2==colnames(tr.mat)[j] )
  tmp$from <- rownames(tr.mat)[i]
  tmp$to   <- rownames(tr.mat)[j]
  new <- rbind( new, tmp )
  }
}
new <- new[,-grep("lex.status",names(new))]
names( new )[grep(timescale,names(new))] <- "Tstart"
new$Tstop <- new$Tstart + new$lex.deltat
names( new )[grep("lex.deltat",names(new))] <- "Y"
class( new ) <- c( "MSana.Lexis","data.frame" )
return( new[order(new$lex.id,new$Tstart),] )
}
